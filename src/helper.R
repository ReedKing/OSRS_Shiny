# Libraries
library(jsonlite)
library(forecast)
library(prophet)
library(ggplot2)
library(ggthemes)
library(plotly)
library(scales)
library(ggfortify)
library(here)

# Functions
capitalize <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep = "", collapse = " ")
}

forecast_plot <- function(data, fcst, title) {
  # Borrowed from https://github.com/inertia7/timeSeries_sp500_R
  # Apparently originally the work of Drew Schmidt @ http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/
  # Heavily retooled from above

  lenx <- length(fcst$x)
  lenmn <- length(fcst$mean)

  df <- data.frame(
    date = data$date,
    x = c(fcst$x, fcst$mean),
    x2 = c(data$price),
    actuals = c(rep(NA, lenx), tail(daily_df$price, n = lenmn)),
    forecast = c(rep(NA, lenx), fcst$mean),
    low1 = c(rep(NA, lenx), fcst$lower[, 1]),
    upp1 = c(rep(NA, lenx), fcst$upper[, 1]),
    low2 = c(rep(NA, lenx), fcst$lower[, 2]),
    upp2 = c(rep(NA, lenx), fcst$upper[, 2])
  )

  ggplot(df, aes(x = date, y = x)) +
    geom_ribbon(aes(ymin = low2, ymax = upp2), fill = "yellow", na.rm = TRUE) +
    geom_ribbon(aes(ymin = low1, ymax = upp1), fill = "orange", na.rm = TRUE) +
    geom_line(data = df, aes(date, x2), color = "red") +
    geom_line(colour = "turquoise4", size = 1) +
    geom_line(data = df[!is.na(df$forecast), ], aes(date, forecast), color = "blue", na.rm = TRUE) +
    geom_line(data = df[!is.na(df$actuals), ], aes(date, actuals), color = "red", na.rm = TRUE) +
    labs(x = 'Date', y = 'Price', title = title) +
    theme_hc()
}