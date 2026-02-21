# Consumer Tech Retail Forecasting Dashboard

An end-to-end retail analytics application built with **R Shiny**, **MongoDB**, and the `forecast` / `ets` packages. Designed as a portfolio piece for data science roles with a retail focus.

## Features

| Tab | Description |
|-----|-------------|
| **Overview** | KPI value boxes, monthly revenue trend, category donut chart, regional bar chart |
| **EDA** | Stacked area chart, weekly units box plots, discount-vs-revenue scatter, brand ranking, seasonality heatmap |
| **Forecasting** | Holt-Winters / ETS point forecast with 95% CI ribbon; configurable by category, region, and horizon |
| **Data** | Filterable DT table with MongoDB query preview and CSV download |

## Architecture

```
MongoDB Atlas (retail_analytics.consumer_tech_sales)
        │
        ▼  mongolite
  global.R  ─── load_data()
        │
        ▼
   app.R (bslib UI + server)
        │
        ├── Overview   (plotly)
        ├── EDA        (plotly)
        ├── Forecasting (forecast::ets + plotly)
        └── Data       (DT + downloadHandler)
```

## Data Schema

| Field | Type | Description |
|-------|------|-------------|
| `date` | string (YYYY-MM-DD) | Week start date |
| `year` | int | Calendar year |
| `month` | int | Month number |
| `week` | int | ISO week number |
| `category` | string | Product category |
| `brand` | string | Brand name |
| `region` | string | Sales region |
| `units_sold` | int | Units sold that week |
| `avg_price` | float | Average selling price (USD) |
| `discount_pct` | float | Discount rate (0–0.20) |
| `revenue` | float | Gross revenue (USD) |

## Quickstart

### 1. Prerequisites

```r
install.packages(c(
  "shiny", "bslib", "plotly", "dplyr", "tidyr",
  "lubridate", "forecast", "mongolite", "DT", "scales"
))
```

### 2. MongoDB setup (optional)

Skip this step to run with the bundled CSV fallback.

```bash
# Set your connection string (MongoDB Atlas free tier works great)
export MONGO_URI="mongodb+srv://<user>:<password>@<cluster>.mongodb.net"
export MONGO_DB="retail_analytics"

# Seed the database
Rscript data/generate_data.R
```

### 3. Run locally

```r
shiny::runApp("shiny-app")
```

### 4. Deploy to shinyapps.io

```r
library(rsconnect)
rsconnect::setAccountInfo(name = "<account>", token = "<token>", secret = "<secret>")
rsconnect::deployApp(
  appDir    = "shiny-app",
  appName   = "consumer-tech-retail-dashboard",
  appFiles  = c("app.R", "global.R", "data/consumer_tech_sales.csv")
)
```

Set `MONGO_URI` and `MONGO_DB` as environment variables in the shinyapps.io dashboard under **App Settings → Environment Variables**.

## Dataset

Synthetic weekly sales data spanning **Jan 2022 – Dec 2024** across:

- **6 categories**: Smartphones, Laptops, Tablets, Wearables, Smart Home, Gaming  
- **12 brands**: Apple, Samsung, Google, OnePlus, Dell, HP, Lenovo, Microsoft, Fitbit, Garmin, Sony, Nintendo, Razer, Philips, Amazon  
- **4 regions**: North America, Europe, Asia-Pacific, Latin America  
- **~15,600 weekly records**  

Seasonal patterns mirror real consumer electronics cycles (Q4 holiday spike, Q1 dip). YoY growth of ~8% is baked in.

## Forecasting Methodology

The app uses `forecast::ets()` (Error, Trend, Seasonal) which automatically selects the best exponential-smoothing model among additive / multiplicative / damped variants. A 95% prediction interval is computed analytically from the fitted state-space model.

For the pre-computed preview on the portfolio page, a pure-Python Holt-Winters triple exponential-smoothing implementation was used.
