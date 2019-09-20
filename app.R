library(shinydashboard)
library(shiny)
library(alphavantager)
library(data.table)
library(plotly)
library(tidyquant)
todayDate = Sys.Date()
av_api_key(apikey)
ui <- dashboardPage(
    dashboardHeader(title = "Stock Dashboard"),
    dashboardSidebar(),
    dashboardBody( 
      tabsetPanel(
        id = "tabs",
        tabPanel(
          title = "Main Dashboard",
          value = "page1",
      fluidRow(
        valueBoxOutput("vbox"),
        textInput("stock", label = "Stock Input", ""),
        actionButton("startAPI", "Request Data from API (limit 5 per minute, 500 perday)"),
        dateRangeInput("dateRange_1", label="Select Date Range", 
                       start = todayDate-40, # Set the initial Start Date 
                       end = todayDate+1,  # Set the initial End Date
                       max = todayDate+1)
    ),
    fluidRow(
        verbatimTextOutput("value"),
        box(
        plotlyOutput("plot_1"))
        ,
        box(
          plotOutput("plot_2")
      )
    )
  )
)
)
)


server <- function(session,input, output) {
  tab_list <- NULL
    output$vbox <- renderValueBox({
        valueBox(
            value = as.character(input$stock),
            subtitle = "Stock Chosen",
            icon = icon("chart-line")
        )
    })
    
    inputData1 <- eventReactive((input$startAPI),{
        av_get(symbol = as.character(input$stock),  av_fun = "TIME_SERIES_DAILY", outputsize = "full")
    })
    

    
    outputData2 <- reactive({
      inputData1() %>%
        filter(timestamp> input$dateRange_1[1] & timestamp < input$dateRange_1[2] )
    })
    
    ## Value Plotted
    observeEvent(input$startAPI != "", {
    tab_title <- paste(input$stock)
    outputData1 <- reactive({
      inputData1() %>%
        filter(timestamp> input$dateRange_1[1] & timestamp < input$dateRange_1[2] ) %>%
        mutate(avg = (low + high + open + close)/4) %>%
        melt(id="timestamp")%>%
        filter(variable == "close")%>%
        setnames(old=c("timestamp","variable","value"), new=c("TimeStamp", "Variable", "Value"))
    })
    if (!(tab_title %in% tab_list)) {
      appendTab(
        inputId = "tabs",
        tabPanel(
          tab_title,
          output$plot_1 <- renderPlotly({
            ggplot(outputData1(), aes(x=TimeStamp,y=Value))+
              geom_line()+
              stat_smooth(method = "lm", col = "red")+
              theme_light()+
              labs(x="",y="Closing Price ($)", title = paste0("Average Valuation for ",input$stock))
          })
          ),
        session = getDefaultReactiveDomain()
      )
      tab_list <<- c(tab_list, tab_title)
    }
    updateTabsetPanel(session, "tabs", selected = tab_title)
    },
    ignoreInit = TRUE
    )
   
    output$plot_2 <- renderPlot({
      outputData2() %>%
      ggplot(aes(x = timestamp, y = close)) +
        geom_line()+
        stat_smooth(method = "lm", col = "red")+
        geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
        geom_ma(ma_fun = SMA, color = "darkblue",size=1)+
        labs(title = paste0("Candlestick Chart for ", input$stock), y = "Closing Price ($)", x = "") +
        theme_tq()
    })

    output$table_1 <- renderDataTable(outputData1()
    )
    output$table_2 <- renderDataTable(outputData2()
    )
    }
shinyApp(ui, server)