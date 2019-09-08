library(shiny)
library(shinydashboard)
library(DT)
library(stringr)
library(tidyr)
library(tibble)
library(dplyr)
library(plyr)
library(ggplot2)
library(plotly)

# Define the fields we want to save from the form
fields <- c("stockName", "dataSize", "startDate","endDate")

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
  box(
    textInput("stockName", "Stock Ticker", ""),
    selectInput("dataSize", "Full or Partial Data?",choice= c("Full","Partial"), FALSE),
    dateInput("startDate", label="Select Start Date", 
                   value = Sys.Date()-180, # Set the initial Start Date 
                   max = Sys.Date()-1),
    dateInput("endDate", label = "Select End Data",
              value = Sys.Date(),
              max = Sys.Date()),
    actionButton("submit", "Submit"),
    actionButton("apiCall", "Make API Call with selected Stocks"),
    actionButton("clearConnections","Clear Connections (max 16)"),
    h3(textOutput('connections')),
    actionButton("killSQL1", "Clear Stock Range Table"),
    actionButton("killSQL2", "Clear Stock Value Table"),
    actionButton("makePlot","Make Plot"),
    DT::dataTableOutput("stockPrices", width = 300), tags$hr()
  ), # END BOX
  box(
    DT::dataTableOutput("responses", width = 300), tags$hr()
    ),
  box(
    plotlyOutput("plot")
  )
  ), # END FLUID PAGE
# END SHINY
  server = function(input, output, session) {
    
    output$connections = renderPrint(length(dbListConnections(MySQL())))
    observeEvent(input$clearConnections, {
      killDbConnections()
    })
    observeEvent(input$killSQL1, {
      clearSQLTables1()
    })
    observeEvent(input$killSQL2, {
      clearSQLTables2()
      loadStockData()
    })
    

    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data <- ldply (data, data.frame)
      data <- t(data)
      data <- as.data.frame(data,make.names=FALSE)
      data <- data[-1,]
      print(head(data))
      print(str(data))
      print(names(data))
      print(head(data))
      names(data) <- c("stockName","dataSize","startDate","endDate")
      data$stockName <- as.character(data$stockName)
      data$dataSize <- as.character(data$dataSize)
      data$startDate <- as.character(as.Date(as.numeric(as.character(data$startDate)),origin="1970-01-01"))
      data$endDate <- as.character(as.Date(as.numeric(as.character(data$endDate)), origin="1970-01-01"))
      print(head(data))
      print(str(data))
      print(names(data))
      print(data[1,])
    })

    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
    })
    
observeEvent(input$apiCall,{
      stockData <- loadData()
      for(i in 1:nrow(stockData)){
        n <- tq_get(stockData[i,1],
            get = "stock.prices",
            from = stockData[i,3],
            to = stockData[i,4])
        n$symbol <- stockData[i,1]
        n <- n %>%
          select(symbol, date, open, high, low, close, volume, adjusted)
        db <- dbConnect(MySQL(), dbname = "financeuser", host = "localhost", 
                        port = 3306, user = "root",
                        password = "1Z2s3e4r%")
      dbWriteTable(db, value = n, name = "stocktable_1", append = TRUE)
      }
})


    output$stockPrices <- DT::renderDataTable({
      input$apiCall
      loadStockData()

})
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$submit
      loadData()
    })
    
 plot <-   eventReactive(input$makePlot,{
   dataframed <- loadStockData()
   dataframed$date <- as.Date(dataframed$date)
      dataframed %>%
        ggplot(aes(x = date, y = close, color = symbol)) +
        geom_line()+
        stat_smooth(method = "lm")+
        labs(title = paste0("Linear Reg. Chart for Stocks"), y = "Closing Price ($)", x = "") +
        theme_tq()
})
 
 output$plot <- renderPlotly({
   plot()
 })
    

  }) # END

