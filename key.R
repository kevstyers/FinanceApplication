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

# Load Local

# saveData <- function(data) {
#   data <- as.data.frame(t(data))
#   if (exists("responses")) {
#     responses <<- rbind(responses, data)
#   } else {
#     responses <<- data
#   }
# }
# 
# loadData <- function() {
#   if (exists("responses")) {
#     responses
#   }
# }

# Load SQL
library(RMySQL)
# test connection

options(mysql = list(
  "host" = "localhost",
  "port" = 3306,
  "user" = "user",
  "password" = "1Z2s3e4r%"
))

databaseName <- "financeuser"
table <- "userinput"


saveData <- function(data) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = "financeuser", host = "localhost", 
                  port = 3306, user = "root",
                  password = "1Z2s3e4r%")
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s VALUES ('%s')",
    table, 
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

loadData <- function() {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = "financeuser", host = "localhost", 
                  port = 3306, user = "root",
                  password = "1Z2s3e4r%")
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", "userinput")
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  data$ID <- seq.int(nrow(data))
  dbDisconnect(db)
  data
}
loadStockData <- function() {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = "financeuser", host = "localhost", 
                  port = 3306, user = "root",
                  password = "1Z2s3e4r%")
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", "stocktable_1")
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

## Quick disconnect

lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

library(RMySQL)  

killDbConnections <- function () {
  all_cons <- dbListConnections(MySQL())
  print(all_cons)
  for(con in all_cons)
    +  dbDisconnect(con)
  print(paste(length(all_cons), " connections killed."))
}

## Clear tables
clearSQLTables1 <- function() {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = "financeuser", host = "localhost", 
                  port = 3306, user = "root",
                  password = "1Z2s3e4r%")
  # Construct the fetching query
  query <- sprintf("DELETE FROM userinput;")
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}
clearSQLTables2 <- function() {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = "financeuser", host = "localhost", 
                  port = 3306, user = "root",
                  password = "1Z2s3e4r%")
  # Construct the fetching query
  query <- sprintf("DELETE FROM stocktable_1;")
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}


## LOCAL STORAGE FOR TESTING
outputDir <- "X:/1_GitHub/FinanceApplication/data/"

saveData <- function(data) {
  # data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}

loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}