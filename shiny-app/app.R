# app.R  — Consumer Tech Retail Forecasting Dashboard
# Run locally:  shiny::runApp("shiny-app")
# Deploy:       rsconnect::deployApp("shiny-app")

source("global.R")

# ---------------------------------------------------------------------------
# Data (loaded once at startup)
# ---------------------------------------------------------------------------
df_raw <- load_data()

# Pre-compute monthly aggregate used across multiple tabs
df_monthly <- df_raw %>%
  mutate(month_date = floor_date(date, "month")) %>%
  group_by(month_date, category) %>%
  summarise(
    revenue    = sum(revenue),
    units_sold = sum(units_sold),
    avg_discount = mean(discount_pct),
    .groups = "drop"
  )

df_monthly_total <- df_monthly %>%
  group_by(month_date) %>%
  summarise(revenue = sum(revenue), units_sold = sum(units_sold), .groups = "drop") %>%
  arrange(month_date)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- page_navbar(
  title = tags$span(
    tags$img(src = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/icons/bar-chart-line-fill.svg",
             height = "24px", style = "margin-right:8px; filter:invert(1);"),
    "Consumer Tech Retail Dashboard"
  ),
  theme = bs_theme(
    version   = 5,
    bootswatch = "cosmo",
    primary   = "#4a90e2"
  ),
  fillable = TRUE,

  # ---- Overview ---------------------------------------------------------
  nav_panel(
    "Overview",
    icon = icon("house"),
    layout_columns(
      fill = FALSE,
      col_widths = c(3, 3, 3, 3),
      value_box(
        title    = "Total Revenue (3 yr)",
        value    = textOutput("kpi_rev"),
        showcase = icon("dollar-sign"),
        theme    = "primary"
      ),
      value_box(
        title    = "Units Sold",
        value    = textOutput("kpi_units"),
        showcase = icon("box-seam"),
        theme    = "success"
      ),
      value_box(
        title    = "YoY Growth (2024 vs 2023)",
        value    = textOutput("kpi_yoy"),
        showcase = icon("graph-up-arrow"),
        theme    = "info"
      ),
      value_box(
        title    = "Top Category",
        value    = textOutput("kpi_top_cat"),
        showcase = icon("trophy"),
        theme    = "warning"
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header("Monthly Revenue Trend"),
        plotlyOutput("plot_revenue_trend", height = "360px")
      ),
      card(
        card_header("Revenue by Category"),
        plotlyOutput("plot_cat_donut", height = "360px")
      )
    ),
    card(
      card_header("Revenue by Region"),
      plotlyOutput("plot_region_bar", height = "280px")
    )
  ),

  # ---- EDA -------------------------------------------------------------
  nav_panel(
    "EDA",
    icon = icon("magnifying-glass-chart"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Stacked Monthly Revenue by Category"),
        plotlyOutput("plot_stacked_area", height = "340px")
      ),
      card(
        card_header("Weekly Units Sold Distribution"),
        plotlyOutput("plot_units_box", height = "340px")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Discount Rate vs Revenue (by Category)"),
        plotlyOutput("plot_discount_scatter", height = "340px")
      ),
      card(
        card_header("Brand Revenue Ranking"),
        plotlyOutput("plot_brand_bar", height = "340px")
      )
    ),
    card(
      card_header("Monthly Seasonality Heatmap (Revenue Index vs. Annual Mean)"),
      plotlyOutput("plot_seasonality_heatmap", height = "260px")
    )
  ),

  # ---- Forecasting -------------------------------------------------------
  nav_panel(
    "Forecasting",
    icon = icon("chart-line"),
    card(
      card_header("Forecast Settings"),
      layout_columns(
        col_widths = c(4, 4, 4),
        selectInput("fc_category", "Category",
                    choices   = c("All", sort(unique(df_raw$category))),
                    selected  = "All"),
        selectInput("fc_region", "Region",
                    choices   = c("All", sort(unique(df_raw$region))),
                    selected  = "All"),
        sliderInput("fc_horizon", "Forecast Horizon (months)",
                    min = 3, max = 12, value = 6, step = 1)
      )
    ),
    card(
      card_header("Revenue Forecast with 95% Confidence Interval (Holt-Winters / ETS)"),
      plotlyOutput("plot_forecast", height = "420px")
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title    = "Model",
        value    = textOutput("fc_model_name"),
        showcase = icon("cpu"),
        theme    = "secondary"
      ),
      value_box(
        title    = "RMSE",
        value    = textOutput("fc_rmse"),
        showcase = icon("bullseye"),
        theme    = "secondary"
      ),
      value_box(
        title    = "MAPE",
        value    = textOutput("fc_mape"),
        showcase = icon("percent"),
        theme    = "secondary"
      )
    )
  ),

  # ---- Raw Data ----------------------------------------------------------
  nav_panel(
    "Data",
    icon = icon("table"),
    card(
      card_header(
        "Weekly Sales Records",
        tags$span(
          class = "badge bg-secondary ms-2",
          textOutput("data_row_count", inline = TRUE)
        )
      ),
      layout_columns(
        fill = FALSE,
        col_widths = c(3, 3, 3, 3),
        selectInput("dt_category", "Category",
                    choices = c("All", sort(unique(df_raw$category)))),
        selectInput("dt_brand",    "Brand",
                    choices = c("All", sort(unique(df_raw$brand)))),
        selectInput("dt_region",   "Region",
                    choices = c("All", sort(unique(df_raw$region)))),
        dateRangeInput("dt_dates", "Date Range",
                       start = min(df_raw$date),
                       end   = max(df_raw$date),
                       min   = min(df_raw$date),
                       max   = max(df_raw$date))
      ),
      DTOutput("data_table"),
      downloadButton("download_data", "Download Filtered CSV", class = "btn-sm mt-2")
    ),
    card(
      card_header("MongoDB Query (for the active filters)"),
      verbatimTextOutput("mongo_query")
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # -- KPIs ----------------------------------------------------------------
  output$kpi_rev     <- renderText(fmt_dollar(sum(df_raw$revenue)))
  output$kpi_units   <- renderText(fmt_k(sum(df_raw$units_sold)))
  output$kpi_yoy     <- renderText({
    rev23 <- sum(df_raw$revenue[year(df_raw$date) == 2023])
    rev24 <- sum(df_raw$revenue[year(df_raw$date) == 2024])
    paste0(ifelse(rev24 > rev23, "+", ""), round((rev24 - rev23) / rev23 * 100, 1), "%")
  })
  output$kpi_top_cat <- renderText({
    df_raw %>%
      group_by(category) %>%
      summarise(r = sum(revenue), .groups = "drop") %>%
      slice_max(r, n = 1) %>%
      pull(category)
  })

  # -- Overview plots -------------------------------------------------------
  output$plot_revenue_trend <- renderPlotly({
    plot_ly(df_monthly_total, x = ~month_date, y = ~revenue, type = "scatter",
            mode = "lines+markers", line = list(color = "#4a90e2", width = 2.5),
            marker = list(size = 5, color = "#4a90e2"),
            hovertemplate = "%{x|%b %Y}<br>$%{y:,.0f}<extra></extra>") %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Revenue (USD)", tickformat = "$.2s"),
        hovermode = "x unified",
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font = list(family = "Inter, system-ui, sans-serif")
      )
  })

  output$plot_cat_donut <- renderPlotly({
    cat_totals <- df_raw %>% group_by(category) %>% summarise(r = sum(revenue), .groups = "drop")
    plot_ly(cat_totals, labels = ~category, values = ~r, type = "pie", hole = 0.5,
            marker = list(colors = unname(CATEGORY_COLORS[cat_totals$category])),
            hovertemplate = "<b>%{label}</b><br>$%{value:,.0f} (%{percent})<extra></extra>") %>%
      layout(showlegend = TRUE,
             paper_bgcolor = "rgba(0,0,0,0)",
             font = list(family = "Inter, system-ui, sans-serif"))
  })

  output$plot_region_bar <- renderPlotly({
    reg_totals <- df_raw %>% group_by(region) %>% summarise(r = sum(revenue), .groups = "drop") %>%
      arrange(desc(r))
    plot_ly(reg_totals, x = ~region, y = ~r, type = "bar",
            marker = list(color = unname(REGION_COLORS[reg_totals$region])),
            hovertemplate = "%{x}<br>$%{y:,.0f}<extra></extra>") %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Revenue (USD)", tickformat = "$.2s"),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font = list(family = "Inter, system-ui, sans-serif")
      )
  })

  # -- EDA plots -----------------------------------------------------------
  output$plot_stacked_area <- renderPlotly({
    p <- plot_ly()
    for (cat in sort(unique(df_monthly$category))) {
      sub <- df_monthly %>% filter(category == cat) %>% arrange(month_date)
      p <- p %>% add_trace(
        x = sub$month_date, y = sub$revenue,
        name = cat, type = "scatter", mode = "none",
        stackgroup = "one", fillcolor = CATEGORY_COLORS[[cat]],
        hovertemplate = paste0("<b>", cat, "</b><br>%{x|%b %Y}<br>$%{y:,.0f}<extra></extra>")
      )
    }
    p %>% layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Revenue (USD)", tickformat = "$.2s"),
      hovermode = "x unified",
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font = list(family = "Inter, system-ui, sans-serif")
    )
  })

  output$plot_units_box <- renderPlotly({
    df_week_cat <- df_raw %>%
      mutate(week_date = floor_date(date, "week")) %>%
      group_by(week_date, category) %>%
      summarise(units = sum(units_sold), .groups = "drop")

    plot_ly(df_week_cat, x = ~category, y = ~units, color = ~category,
            colors = CATEGORY_COLORS,
            type = "box",
            hovertemplate = "%{x}<br>%{y:,.0f} units<extra></extra>") %>%
      layout(
        xaxis  = list(title = ""),
        yaxis  = list(title = "Weekly Units Sold"),
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font = list(family = "Inter, system-ui, sans-serif")
      )
  })

  output$plot_discount_scatter <- renderPlotly({
    df_cat_weekly <- df_raw %>%
      mutate(week_date = floor_date(date, "week")) %>%
      group_by(week_date, category) %>%
      summarise(avg_disc = mean(discount_pct), revenue = sum(revenue), .groups = "drop")

    plot_ly(df_cat_weekly, x = ~avg_disc, y = ~revenue, color = ~category,
            colors = CATEGORY_COLORS,
            type = "scatter", mode = "markers",
            marker = list(size = 5, opacity = 0.65),
            hovertemplate = "<b>%{text}</b><br>Discount: %{x:.1%}<br>Revenue: $%{y:,.0f}<extra></extra>",
            text = ~category) %>%
      layout(
        xaxis = list(title = "Avg Discount Rate", tickformat = ".0%"),
        yaxis = list(title = "Weekly Revenue (USD)", tickformat = "$.2s"),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font = list(family = "Inter, system-ui, sans-serif")
      )
  })

  output$plot_brand_bar <- renderPlotly({
    brand_totals <- df_raw %>%
      group_by(brand) %>%
      summarise(r = sum(revenue), .groups = "drop") %>%
      arrange(r)

    plot_ly(brand_totals, x = ~r, y = ~brand, type = "bar", orientation = "h",
            marker = list(color = "#4a90e2"),
            hovertemplate = "<b>%{y}</b><br>$%{x:,.0f}<extra></extra>") %>%
      layout(
        xaxis = list(title = "Revenue (USD)", tickformat = "$.2s"),
        yaxis = list(title = ""),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font = list(family = "Inter, system-ui, sans-serif")
      )
  })

  output$plot_seasonality_heatmap <- renderPlotly({
    monthly_index <- df_monthly_total %>%
      mutate(year  = year(month_date),
             month = month(month_date, label = TRUE, abbr = TRUE)) %>%
      group_by(year) %>%
      mutate(annual_mean = mean(revenue),
             index = revenue / annual_mean) %>%
      ungroup()

    plot_ly(monthly_index,
            x = ~month, y = ~as.factor(year), z = ~index,
            type = "heatmap", colorscale = "RdBu",
            reversescale = TRUE,
            zmin = 0.4, zmax = 1.8,
            hovertemplate = "%{y} %{x}<br>Index: %{z:.2f}<extra></extra>") %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        paper_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Inter, system-ui, sans-serif")
      )
  })

  # -- Forecasting ----------------------------------------------------------
  fc_series <- reactive({
    sub <- df_raw
    if (input$fc_category != "All") sub <- sub %>% filter(category == input$fc_category)
    if (input$fc_region    != "All") sub <- sub %>% filter(region    == input$fc_region)

    monthly <- sub %>%
      mutate(month_date = floor_date(date, "month")) %>%
      group_by(month_date) %>%
      summarise(revenue = sum(revenue), .groups = "drop") %>%
      arrange(month_date)

    ts_data <- ts(monthly$revenue, frequency = 12,
                  start = c(year(min(monthly$month_date)),
                             month(min(monthly$month_date))))

    fit <- ets(ts_data)
    fc  <- forecast(fit, h = input$fc_horizon, level = 95)

    list(
      monthly  = monthly,
      ts_data  = ts_data,
      fit      = fit,
      fc       = fc,
      rmse     = round(sqrt(mean(residuals(fit)^2)), 0),
      mape     = round(mean(abs(residuals(fit) / ts_data)) * 100, 2)
    )
  })

  output$plot_forecast <- renderPlotly({
    res <- fc_series()

    fc_dates <- seq.Date(
      from = floor_date(max(res$monthly$month_date), "month") %m+% months(1),
      by   = "month",
      length.out = input$fc_horizon
    )

    plot_ly() %>%
      # Historical
      add_trace(x = res$monthly$month_date, y = res$monthly$revenue,
                type = "scatter", mode = "lines",
                name = "Historical",
                line = list(color = "#4a90e2", width = 2),
                hovertemplate = "Historical<br>%{x|%b %Y}<br>$%{y:,.0f}<extra></extra>") %>%
      # CI ribbon (upper)
      add_trace(x = fc_dates, y = as.numeric(res$fc$upper),
                type = "scatter", mode = "none",
                name = "95% CI Upper", fill = "none",
                fillcolor = "rgba(74,144,226,0.15)",
                line = list(color = "transparent"),
                showlegend = FALSE,
                hoverinfo = "skip") %>%
      # CI ribbon (lower — filled to upper)
      add_trace(x = fc_dates, y = as.numeric(res$fc$lower),
                type = "scatter", mode = "none",
                name = "95% CI", fill = "tonexty",
                fillcolor = "rgba(74,144,226,0.15)",
                line = list(color = "transparent"),
                hovertemplate = "95% CI<br>%{x|%b %Y}<br>$%{y:,.0f}<extra></extra>") %>%
      # Point forecast
      add_trace(x = fc_dates, y = as.numeric(res$fc$mean),
                type = "scatter", mode = "lines+markers",
                name = "Forecast",
                line = list(color = "#e25c4a", width = 2.5, dash = "dot"),
                marker = list(size = 7, color = "#e25c4a"),
                hovertemplate = "Forecast<br>%{x|%b %Y}<br>$%{y:,.0f}<extra></extra>") %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Revenue (USD)", tickformat = "$.2s"),
        hovermode = "x unified",
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font = list(family = "Inter, system-ui, sans-serif"),
        legend = list(orientation = "h", yanchor = "bottom", y = 1.02, xanchor = "right", x = 1)
      )
  })

  output$fc_model_name <- renderText({
    res <- fc_series()
    paste("ETS(", paste(res$fit$components[1:3], collapse = ","), ")")
  })

  output$fc_rmse <- renderText(fmt_dollar(fc_series()$rmse))
  output$fc_mape <- renderText(paste0(fc_series()$mape, "%"))

  # -- Data tab ------------------------------------------------------------
  filtered_data <- reactive({
    sub <- df_raw
    if (input$dt_category != "All") sub <- sub %>% filter(category == input$dt_category)
    if (input$dt_brand    != "All") sub <- sub %>% filter(brand    == input$dt_brand)
    if (input$dt_region   != "All") sub <- sub %>% filter(region   == input$dt_region)
    sub %>% filter(date >= input$dt_dates[1], date <= input$dt_dates[2]) %>%
      arrange(desc(date))
  })

  output$data_row_count <- renderText(paste(nrow(filtered_data()), "rows"))

  output$data_table <- renderDT({
    filtered_data() %>%
      mutate(
        revenue    = dollar(revenue, accuracy = 1),
        avg_price  = dollar(avg_price, accuracy = 0.01),
        discount_pct = percent(discount_pct, accuracy = 0.1)
      ) %>%
      datatable(
        options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE,
        class    = "table table-sm table-hover"
      )
  })

  output$download_data <- downloadHandler(
    filename = function() paste0("consumer_tech_sales_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(filtered_data(), file, row.names = FALSE)
  )

  output$mongo_query <- renderText({
    filters <- list()
    if (input$dt_category != "All")
      filters <- c(filters, list(sprintf('"category": "%s"', input$dt_category)))
    if (input$dt_brand != "All")
      filters <- c(filters, list(sprintf('"brand": "%s"', input$dt_brand)))
    if (input$dt_region != "All")
      filters <- c(filters, list(sprintf('"region": "%s"', input$dt_region)))
    filters <- c(
      filters,
      list(sprintf('"date": {"$gte": "%s", "$lte": "%s"}',
                   input$dt_dates[1], input$dt_dates[2]))
    )
    paste0(
      'db.consumer_tech_sales.find({\n  ',
      paste(filters, collapse = ",\n  "),
      '\n})'
    )
  })
}

# ---------------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
