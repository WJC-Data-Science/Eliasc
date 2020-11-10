library(tidyquant)
library(tidyverse)
library(dplyr)
library(lubridate)
library(timetk)
library(dygraphs)

tickers_today <- c("QQQ", "MO", "KO", "Abb", "LH", "PFE", "cinf")
start <- today() - years(5)

todays_prices <- tq_get(tickers_today, get = 'stock.prices', from = start)
head(todays_prices)

matrix_prices <- todays_prices %>% 
  select(symbol, date, adjusted) %>% 
  pivot_wider(names_from = symbol, values_from = adjusted) %>% 
  tk_xts(date_var = date)
head(matrix_prices)

## Overall Stock Price For the Past 5 Years
dygraph(matrix_prices, main = 'Tickers Today') %>%
  dySeries(fillGraph = FALSE) %>% 
  dyHighlight(highlightCircleSize = 5., 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) %>% 
  dyOptions(axisLineColor = "black",
            gridLineColor = "black") %>% 
  dyRangeSelector() 

## Percent Change
perc_change <- todays_prices %>% 
  group_by(symbol) %>% 
  mutate(percent_change_total = (adjusted/adjusted[1]-1)*100)

head(perc_change)

matrix_prices_perc <- perc_change %>% 
  select(symbol, date, percent_change_total) %>% 
  pivot_wider(names_from = symbol, values_from = percent_change_total) %>% 
  tk_xts(date_var = date)

dygraph(matrix_prices_perc, main = 'Percent Change') %>%
  dySeries(fillGraph = FALSE) %>% 
  dyHighlight(highlightCircleSize = 5., 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) %>% 
  dyOptions(axisLineColor = "black",
            gridLineColor = "black") %>% 
  dyRangeSelector()

## If $25,000 were invested into each of the stocks 5 years ago
dygraph(matrix_prices, main = 'If $25,000 Were Invested Into Each 5 Years Ago') %>% 
  dySeries(fillGraph = FALSE) %>% 
  dyHighlight(highlightCircleSize = 5., 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) %>%
  dyRebase(value = 25000)


## Volume
volumes <- todays_prices %>% 
  group_by(symbol, date, volume) %>% 
  select(symbol, date, volume)
head(volumes)


ggplot() +
  geom_line(data = volumes, mapping = aes(x = date, y = volume, color = symbol)) +
  facet_wrap(~symbol, nrow = 3) +
  labs(x = 'Date', y = 'Volume', title = 'Volumes For Each Stock') +
  theme_bw() +
  theme(legend.position = 'none')





