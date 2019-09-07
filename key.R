require('alphavantager')
require('ggplot2')
require('plotly')
require('reshape2')
require('substr')

apikey = "IVPFD2A7U308JZCZ"


av_api_key(apikey)

inputStock15min <- av_get(symbol = "VGT",  av_fun = "TIME_SERIES_INTRADAY", interval = "15min", outputsize = "full")
inputStock15min$date <- substr(inputStock15min$timestamp,1,nchar(inputStock15min$timestamp)-0)
meltedStocks <- inputStock15min %>%
  melt(id = c("timestamp","date"))



todaysMelt <- meltedStocks %>%
  filter(date >= "2019-09-06")


stockPlot <- ggplot(data = todaysMelt %>% filter(variable != "volume"), aes(x=timestamp, y=value, color=variable))+
  geom_line()
ggplotly(stockPlot)
