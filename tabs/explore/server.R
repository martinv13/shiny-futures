
## Explore Tab Server
exploreTab <- function (input, output, session) {
  
  ns <- session$ns
  
  # a "Contracts" R6Class
  series <- callModule(selContracts, "selContractsList")
  
  selRange <- callModule(plotSeries, "graphPanel", series)
  
  seriesSummary <- reactive({
    ser <- series()
    ser$series
  })
  
  callModule(performanceStats, "perfStats", seriesSummary, selRange)

  db <- Db$new()$singleton
  
  observeEvent(input$clearStratsCache, {
    res <- dbSendQuery(db$localDBI, "delete from strats_cache")
    dbClearResult(res)
    showNotification("Cache cleared")
  })

  
  # observe({
  #   print(as.POSIXct(input$graphClicked/1000, origin="1970-01-01"))
  #   print("hello")
  # })
  
}

  