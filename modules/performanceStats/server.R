
## performanceStats module Server

tempStat <- NULL

performanceStats <- function (input, output, session, seriesData=NULL, selRange=NULL) {
  
  ns <- session$ns
  rv <- reactiveValues(range=NULL,
                       series=NULL)
  
  perfCalc <- Performance$new()
  
  observe({
    if (!is.null(selRange)) {
      range <- selRange()
      if (!is.null(range)) range <- as.Date(range)+1
      if (!identical(range, isolate(rv$range))) {
        rv$range <- range
      }}})
  observe({ 
    if (!is.null(seriesData)) {
      rv$series <- seriesData()
      if (!is.null(rv$series)) {
        range <- as.Date(c(min(rv$series$Date), max(rv$series$Date)))
        if (!identical(range, isolate(rv$range))) {
          rv$range <- range
        }}
      tempStat <<- rv$series
    }})
  
  output$rangeOutput <- renderText({
    if (!is.null(rv$range)) {
      paste("Values calculated from", strftime(rv$range[1], format = "%Y-%m-%d"),
            "to", strftime(rv$range[2], format = "%Y-%m-%d"))
    } else {
      NULL
    }
  })
  
  output$statistics <- renderTable({
    if (!is.null(rv$series)) {
      perfCalc$series <- rv$series
      perfCalc$range <- rv$range
      perfCalc$metrics <- input$options
      perfCalc$retOption <- input$returnsType
      perfCalc$results
    } else {
      NULL
    }
  })
  
  output$main <- renderUI({
    if (is.null(rv$series)) {
      tags$p("No data selected")
    } else {
      tagList(
        textOutput(ns("rangeOutput")),
        radioButtons(ns("returnsType"), NULL, 
                     choices=c("daily returns"="daily", 
                               "10-day rolling returns"="rolling", 
                               "period returns"="period"), inline=TRUE),
        tags$div(style = "max-height:82vh;overflow-y:auto;",
                 tableOutput(ns("statistics"))),
        checkboxGroupInput(ns("options"), NULL,
                           choices = c("Compute correlations"="correl", "Compute annual returns"="annual"),
                           inline = TRUE))
    }
  })
}