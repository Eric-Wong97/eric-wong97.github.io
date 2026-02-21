#!/usr/bin/env Rscript
# data/generate_data.R
#
# Generates synthetic consumer-tech retail data and inserts it into MongoDB.
# Run once before starting the Shiny app against a live database.
#
# Usage:
#   Rscript data/generate_data.R
#
# Environment variables:
#   MONGO_URI  — MongoDB connection string (default: mongodb://localhost:27017)
#   MONGO_DB   — database name (default: retail_analytics)

library(mongolite)
library(dplyr)
library(lubridate)

MONGO_URI <- Sys.getenv("MONGO_URI", "mongodb://localhost:27017")
MONGO_DB  <- Sys.getenv("MONGO_DB",  "retail_analytics")
MONGO_COL <- "consumer_tech_sales"

set.seed(42)

# ---------------------------------------------------------------------------
# Schema
# ---------------------------------------------------------------------------
categories <- list(
  Smartphones = list(brands = c("Apple", "Samsung", "Google", "OnePlus"),
                     base_price = 850, base_units = 420),
  Laptops     = list(brands = c("Apple", "Dell", "HP", "Lenovo", "Microsoft"),
                     base_price = 1100, base_units = 210),
  Tablets     = list(brands = c("Apple", "Samsung", "Lenovo", "Microsoft"),
                     base_price = 620, base_units = 140),
  Wearables   = list(brands = c("Apple", "Samsung", "Fitbit", "Garmin"),
                     base_price = 280, base_units = 310),
  `Smart Home`= list(brands = c("Amazon", "Google", "Samsung", "Philips"),
                     base_price = 150, base_units = 260),
  Gaming      = list(brands = c("Sony", "Microsoft", "Nintendo", "Razer"),
                     base_price = 450, base_units = 180)
)
regions <- c("North America", "Europe", "Asia-Pacific", "Latin America")
region_mult <- c(`North America` = 1.0, Europe = 0.85,
                 `Asia-Pacific`  = 1.15, `Latin America` = 0.55)

# Weekly dates: 2022-01-03 → 2024-12-28
weeks <- seq.Date(as.Date("2022-01-03"), as.Date("2024-12-28"), by = "week")

# ---------------------------------------------------------------------------
# Generate rows
# ---------------------------------------------------------------------------
rows <- lapply(weeks, function(d) {
  m   <- month(d)
  yr  <- year(d)
  yrs <- as.numeric(d - as.Date("2022-01-03")) / 365.25
  trend    <- 1 + 0.08 * yrs
  seasonal <- 1 + 0.45 * sin((m - 11) * pi / 6)
  if (m %in% c(11, 12)) seasonal <- seasonal * 1.25
  if (m %in% c(1, 2))   seasonal <- seasonal * 0.82

  do.call(rbind, lapply(names(categories), function(cat) {
    meta <- categories[[cat]]
    do.call(rbind, lapply(meta$brands, function(brand) {
      do.call(rbind, lapply(regions, function(region) {
        rm         <- region_mult[[region]]
        units_base <- meta$base_units / length(meta$brands)
        units      <- max(1L, as.integer(units_base * seasonal * trend * rm * rnorm(1, 1, 0.12)))
        avg_price  <- meta$base_price * rnorm(1, 1, 0.04)
        discount   <- round(runif(1, 0, 0.20), 3)
        revenue    <- round(units * avg_price * (1 - discount), 2)
        data.frame(
          date         = as.character(d),
          year         = yr,
          month        = m,
          week         = isoweek(d),
          category     = cat,
          brand        = brand,
          region       = region,
          units_sold   = units,
          avg_price    = round(avg_price, 2),
          discount_pct = discount,
          revenue      = revenue,
          stringsAsFactors = FALSE
        )
      }))
    }))
  }))
})
df <- do.call(rbind, rows)
message(sprintf("Generated %d rows", nrow(df)))

# ---------------------------------------------------------------------------
# Write to MongoDB
# ---------------------------------------------------------------------------
con <- mongo(collection = MONGO_COL, db = MONGO_DB, url = MONGO_URI)
con$drop()
con$insert(df)
count <- con$count()
con$disconnect()
message(sprintf("Inserted %d documents into %s.%s", count, MONGO_DB, MONGO_COL))

# Also (re)write the bundled CSV as a fallback
write.csv(df, file.path(dirname(sys.frame(1)$ofile), "consumer_tech_sales.csv"),
          row.names = FALSE)
message("Refreshed data/consumer_tech_sales.csv")
