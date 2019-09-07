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
      fluidRow(
        valueBoxOutput("vbox"),
        textInput("stock", label = "Stock Input", ""),
        actionButton("startAPI", "Request Data from API (limit 5 per minute, 500 perday)"),
        dateRangeInput("dateRange_1", label="Select Date Range", 
                       start = todayDate-180, # Set the initial Start Date 
                       end = todayDate+1,  # Set the initial End Date
                       max = todayDate+1)
    ),
    fluidRow(
        verbatimTextOutput("value"),
        plotlyOutput("plot_1"),
        dataTableOutput("table_1")
    )
    )
)

server <- function(input, output) {
    output$vbox <- renderValueBox({
        valueBox(
            value = as.character(input$stock),
            subtitle = "Stock Chosen",
            icon = icon("chart-line")
        )
    })
    
    inputData1 <- eventReactive((input$startAPI),{
        av_get(symbol = as.character(input$stock),  av_fun = "TIME_SERIES_DAILY", interval = "15min", outputsize = "full")
    })
    
    outputData1 <- reactive({
        inputData1() %>%
            mutate(date = substr(timestamp,1,nchar(timestamp)-5))%>%
            filter(timestamp> input$dateRange_1[1] & timestamp < input$dateRange_1[2] ) %>%
            mutate(avg = (low + high + open + close)/4) # %>%
            #melt(id=c("timestamp","date"))%>%
            #filter(variable%in% c("open","avg","close"))%>%
            #setnames(old=c("timestamp","date","variable","value"), new=c("TimeStamp", "Date", "Variable", "Value"))
    })

    ## Value Plotted
    output$plot_1 <- renderPlotly({
        ggplot(outputData1(), aes(x=timestamp,y=avg))+
            geom_line()+
            stat_smooth(method = "lm", col = "red")+
            theme_light()+
            labs(x="Date",y="Stock Value ($)", title = paste0("Average Valuation for ",input$stock))
    })

    output$table_1 <- renderDataTable(outputData1()
    )
    }
shinyApp(ui, server)