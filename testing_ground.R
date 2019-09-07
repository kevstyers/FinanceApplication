library(tidyquant)
# Testing Area
apikey = "IVPFD2A7U308JZCZ"
av_api_key(apikey)
inputData1 <- av_get(symbol = "CAT",  av_fun = "TIME_SERIES_DAILY", interval = "15min", outputsize = "compact")

Test<- inputData1 %>%
  mutate(avg = (low + high + open + close)/4) # %>%

tplot <- ggplot(inputData1, aes(x = timestamp, y = close)) +
  geom_barchart(aes(open = open, high = high, low = low, close = close)) +
  labs(x="Date",y="Stock Value ($)") + 
  theme_tq()
ggplotly(tplot)

Test %>%
  ggplot(aes(x = timestamp, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "AAPL Candlestick Chart", y = "Closing Price", x = "") +
  theme_tq()
