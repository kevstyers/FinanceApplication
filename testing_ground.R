library(tidyquant)
# Testing Area
apikey = "IVPFD2A7U308JZCZ"
av_api_key(apikey)
inputData1 <- av_get(symbol = "CAT",  av_fun = "TIME_SERIES_DAILY", interval = "15min", outputsize = "compact")

as.numeric(inputData1$timestamp[50] - inputData1$timestamp[1])

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



observeEvent(input$bar_clicked, {
  airport <- input$bar_clicked
  month <- input$month
  tab_title <- paste(
    input$airline, "-", airport,
    if (month != 99) {
      paste("-", month.name[as.integer(month)])
    }
  )
  if (!(tab_title %in% tab_list)) {
    appendTab(
      inputId = "tabs",
      tabPanel(
        tab_title,
        DT::renderDataTable(
          # This function return a data.frame with
          # the top 100 records of that airport
          get_details(airport = airport) 
        )
      )
    )
    
    tab_list <<- c(tab_list, tab_title)
  }
  updateTabsetPanel(session, "tabs", selected = tab_title)
})


