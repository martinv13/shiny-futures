
# performanceTable module server

performanceTable <- function (input, output, session, seriesData, range = NULL) {

  ns <- session$ns
  
  values <- reactiveValues(series=NULL, 
                           range=NULL,
                           rangeScale=as.Date("2000-01-01"),
                           params=list(scaled=FALSE, 
                                       log=TRUE,
                                       roller=1,
                                       rollpoints=FALSE,
                                       positions=FALSE))
  
  observe({
    values$params <- list(log=input$dyLog,
                          roller=input$dyRoller,
                          rollpoints=input$rollPoints,
                          scaled=input$fixStart,
                          positions=input$positions)
  })
  
  debouncedRange <- debounce({input$dygraph1_date_window}, 600)
  
  observe({
    ra <- debouncedRange()
    if(!is.null(ra)) {
      values$range <- as.POSIXct(ra) + 24*3600
    }
    if (!is.null(values$params$scaled) && values$params$scaled) {
      values$rangeScale <- values$range[1]
    }
  })
  
  output$dygraph1 <- renderDygraph({
    graph <- values$series
    params <- values$params
    dy <- NULL 
    if (!is.null(graph)) {
      graphseries <- graph$series %>% arrange(Date)
      cols <- graphseries %>% select(-Date)
      series <- xts(cols, 
                    order.by = graphseries$Date)
      if (params$scaled) {
        scales <- unlist(lapply(graphseries %>%
                           filter(Date >= as.Date(values$rangeScale)) %>%
                           select(-Date),
                           function(x) x[match(TRUE, !is.na(x))[1]]))/100
        series %<>% scale(center=rep(0, length(scales)), scale=scales)
      }
      dyEpl <- dygraph(series, main = NULL, group="series") %>% 
        dyRangeSelector(dateWindow = isolate(values$range)) %>% 
        dyOptions(logscale=params$log, connectSeparatedPoints=TRUE) %>%
        dyRoller(showRoller=FALSE, rollPeriod=params$roller) %>%
        dyCallbacks(clickCallback="dygraphClick")
      if (params$rollpoints && !is.null(graph$rollpoints)) {
        graph$rollpoints %>%
          ungroup() %>% rowwise() %>%
          do({
            row <- .
            dyEpl <<- dyEpl %>% dyAnnotation(x=as.POSIXct(row$Date), 
                                             text="R", 
                                             tooltip=row$Text,
                                             height=25,
                                             series=row$series)
            data.frame()
          })
      }
      dyEpl %>% dyLegend(show="follow")
    } else {
      NULL
    }
  })
  
  output$dygraph2 <- renderDygraph({
    graph <- values$series
    params <- values$params
    dy <- NULL 
    if (!is.null(graph) && !is.null(graph$rollpoints) && params$positions) {
      rollpoints <- graph$rollpoints %>% 
        select(Date, Position, series) %>%
        spread(series, Position) %>%
        arrange(Date)
      dyEpl <- dygraph(rollpoints, main = NULL, group="series", height = "150px") %>% 
        dyOptions(connectSeparatedPoints=TRUE, stepPlot=TRUE)
      dyEpl %>% dyLegend(show="follow")
    } else {
      NULL
    }
  })
  
  output$graphPanel <- renderUI({
    values$series <- seriesData()
    if (!is.null(values$series)){
      tagList(
        fluidRow(
          column(width=12,
                 dygraphOutput(ns("dygraph1")),
                 dygraphOutput(ns("dygraph2"), height = "150px"))),
        fluidRow(
          column(width = 4, offset=1, style="padding:15px",
                 checkboxInput(ns("dyLog"), "Log scale", TRUE),
                 checkboxInput(ns("rollPoints"), "Display roll points", FALSE),
                 checkboxInput(ns("fixStart"), "Base 100", FALSE),
                 checkboxInput(ns("positions"), "Positions", FALSE)),
          column(width = 3, style="padding:15px",
                 sliderInput(ns("dyRoller"), "Moving average", 1, 50, 1)),
          column(width = 3, style="padding:15px",
                 downloadButton(ns("download"), "CSV"))
          
        )
      )
    } else {
      fluidRow(
        tags$p(alt)
      ) 
    }
  })
  
  download <- function(file) {
    graph <- values$series
    if (!is.null(graph)){
      d <- graph$series
      if (!is.null(values$range)) {
        d %<>% filter(Date>=values$range[1] & Date<=values$range[2])
      }
      d %>% 
        mutate(Date=strftime(Date, format="%d/%m/%Y")) %>%
        write.table(file, sep=";", dec=",", row.names = FALSE)
    }
  }
  output$download <- downloadHandler("series.csv", download)
  
}