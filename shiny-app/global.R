# global.R
# Loaded once when the Shiny app starts.
# Handles library imports, MongoDB connection, and shared data-loading logic.

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(forecast)
library(mongolite)
library(DT)
library(scales)

# ---------------------------------------------------------------------------
# MongoDB configuration
# Set MONGO_URI in your environment (e.g. .Renviron or shinyapps.io secrets).
# The app falls back to the bundled CSV when MongoDB is unavailable.
# ---------------------------------------------------------------------------
MONGO_URI   <- Sys.getenv("MONGO_URI", "mongodb://localhost:27017")
MONGO_DB    <- Sys.getenv("MONGO_DB",  "retail_analytics")
MONGO_COL   <- "consumer_tech_sales"

load_data <- function() {
  tryCatch({
    con <- mongo(collection = MONGO_COL, db = MONGO_DB, url = MONGO_URI)
    df  <- con$find(
      query  = '{}',
      fields = '{"_id":0,"date":1,"year":1,"month":1,"week":1,
                  "category":1,"brand":1,"region":1,
                  "units_sold":1,"avg_price":1,"discount_pct":1,"revenue":1}'
    )
    con$disconnect()
    df$date <- as.Date(df$date)
    message(sprintf("[MongoDB] Loaded %d rows from %s.%s", nrow(df), MONGO_DB, MONGO_COL))
    df
  }, error = function(e) {
    message("[MongoDB] Connection failed — using bundled CSV. Reason: ", conditionMessage(e))
    df <- read.csv(
      file.path("data", "consumer_tech_sales.csv"),
      stringsAsFactors = FALSE
    )
    df$date <- as.Date(df$date)
    df
  })
}

# ---------------------------------------------------------------------------
# Shared palette (matches site --primary and Bootstrap categoricals)
# ---------------------------------------------------------------------------
CATEGORY_COLORS <- c(
  "Smartphones" = "#4a90e2",
  "Laptops"     = "#e25c4a",
  "Tablets"     = "#4ae28c",
  "Wearables"   = "#e2c94a",
  "Smart Home"  = "#a04ae2",
  "Gaming"      = "#4ae2d9"
)

REGION_COLORS <- c(
  "North America" = "#4a90e2",
  "Europe"        = "#63c9a0",
  "Asia-Pacific"  = "#f0a500",
  "Latin America" = "#e2604a"
)

fmt_dollar <- function(x) dollar(x, scale = 1e-6, suffix = "M", accuracy = 0.1)
fmt_k      <- function(x) number(x, scale = 1e-3, suffix = "K", accuracy = 0.1)
