# Libraries
library(jsonlite)
library(forecast)
library(ggplot2)
library(ggthemes)
library(plotly)
library(scales)
library(ggfortify)
library(here)

# Functions
capitalize <- function(x) {
  # Capitalizes first letters of words in string
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep = "", collapse = " ")
}

rs_axis <- function(...) {
  function(x) {
    # Credit to https://stat.ethz.ch/pipermail/r-help/2012-January/299804.html
    # Returns character with label after compressing integer to nearest order of magnitude in cut
    cut <- c(1e0, 1e3, 1e6, 1e9)
    lab <- c('', 'K', 'M', 'B')

    i <- findInterval(abs(x), cut)
    i <- ifelse(i == 0, which(cut == 1e0), i)

    paste0(
      format(round(x / cut[i], 1), trim = TRUE, scientific = FALSE),
      lab[i])
  }
}

forecast_plot <- function(data, fcst, title) {
  # Originally the work of Drew Schmidt @
  # http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/
  # Simplified vs. source

  lenx <- length(fcst$x)
  lenmn <- length(fcst$mean)

  df <- data.frame(
    Date = data$date,
    Price = floor(c(data$price)),
    Forecast = c(rep(NA, lenx), floor(fcst$mean)),
    low1 = c(rep(NA, lenx), fcst$lower[, 1]),
    upp1 = c(rep(NA, lenx), fcst$upper[, 1]),
    low2 = c(rep(NA, lenx), fcst$lower[, 2]),
    upp2 = c(rep(NA, lenx), fcst$upper[, 2])
  )

  ggplot(df, aes(x = Date, y = Price)) +
    geom_ribbon(aes(ymin = low2, ymax = upp2), fill = "yellow", na.rm = TRUE) +
    geom_ribbon(aes(ymin = low1, ymax = upp1), fill = "orange", na.rm = TRUE) +
    geom_line(colour = "turquoise4", size = .8, na.rm = TRUE) +
    geom_line(data = df[!is.na(df$Forecast), c(1, 3)], aes(Date, Forecast), color = "red", na.rm = TRUE) +
    scale_x_date(date_breaks = '1 month', date_labels = "%b %Y") +
    scale_y_continuous(labels = rs_axis()) +
    labs(x = 'Date', y = 'Price', title = title) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          )
}