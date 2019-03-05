# Libraries
library(jsonlite)
library(forecast)
library(data.table)
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
