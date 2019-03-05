# Call libraries & functions
here::here()
source(here::here('src', 'helper.R'))

# URL Strings
base_url <- 'http://services.runescape.com/m=itemdb_oldschool'
ge_endpoint <- '/api/catalogue/detail.json?item='
search_endpoint <- '/api/catalogue/items.json?category=1&alpha=a&page=1' ## note that &alpha=a + page=1 is search decision
graph_endpoint <- '/api/graph/itemID.json'
item_id <- '4151'

# Retrieve Values for item
item_summary <- fromJSON(paste0(base_url, ge_endpoint, item_id))
daily <- fromJSON(paste0(base_url, '/api/graph/', item_id, '.json'), flatten = TRUE)
daily <- do.call(rbind, lapply(daily$daily, paste0))
daily_df <- data.frame(
  date = as.Date(as.POSIXct(
    as.numeric(rownames(daily)) / 1000, origin = '1970-01-01')),
  price = floor(as.integer(daily) / 1000)
)


# Fit models
daily_ts <- as.ts(daily_df$price)

fit_auto_arima <- auto.arima(daily_ts[1:(length(daily_ts) - 7)]) ## 1 week of data to test again
forecast <- forecast(fit_auto_arima, h = 7)

# Graph Models
testplot <- ggplot(daily_df, aes(x = date, y = price)) +
  geom_line(size = .7, color = 'navy') +
  geom_point(size = 1, color = 'navy') +
  scale_x_date(date_breaks = '1 months', date_labels = "%B %Y") +
  scale_y_continuous(labels = function(x) paste0(x, "K")) +
  labs(x = 'Date', y = 'Price', title = paste(capitalize(item_summary$item$name), 'LW Forecast vs. Actuals')) +
  theme_hc()
testplot


##testplotly <- plot_ly(daily_df, x = ~date, y =~price, type = 'scatter', mode = 'lines+markers', name = 'Price') %>%
##  layout(title = paste(capitalize(item_summary$item$name), 'LW Forecast vs. Actuals'))
##testplotly




