
# plotSeries module server

plotSeries <- function (input, output, session, seriesData, alt="Select series to display") {

  ns <- session$ns
  
  rv <- reactiveValues(series=NULL, 
                       range=NULL,
                       rangeScale=as.Date("2000-01-01"),
                       params=c("log"),
                       roller=1,
                       moreOptions="more")
  
  debouncedRange <- debounce({input$dygraph1_date_window}, 600)
  
  observe({
    ra <- debouncedRange()
    if(!is.null(ra)) {
      rv$range <- as.POSIXct(ra) + 24*3600
    }
    if ("fixStart" %in%  rv$params) {
      rv$rangeScale <- rv$range[1]
    }
  })
  
  output$dygraph1 <- renderDygraph({
    graph <- rv$series
    dy <- NULL 
    if (!is.null(graph)) {
      graphseries <- graph$series %>% arrange(Date)
      cols <- graphseries %>% select(-Date, -period)
      series <- xts(cols, 
                    order.by = graphseries$Date)
      if ("fixStart" %in%  rv$params) {
        scales <- unlist(lapply(graphseries %>%
                           filter(Date >= as.Date(rv$rangeScale)) %>%
                           select(-Date, -period),
                           function(x) x[match(TRUE, !is.na(x))[1]]))/100
        series %<>% scale(center=rep(0, length(scales)), scale=scales)
      }
      range <- isolate(rv$range)
      if (!is.null(range)) range <- strftime(range, format="%Y-%m-%d")
      dyEpl <- dygraph(series, main = NULL, group="series") %>% 
        dyRangeSelector(dateWindow = range) %>% 
        dyOptions(logscale="log" %in% rv$params, connectSeparatedPoints=TRUE) %>%
        dyRoller(showRoller=FALSE, rollPeriod=rv$roller) %>%
        dyCallbacks(clickCallback="dygraphClick")
      if (("rollPoints" %in% rv$params) && !is.null(graph$rollpoints)) {
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
    graph <- rv$series
    dy <- NULL 
    if (!is.null(graph) && !is.null(graph$rollpoints) && "positions"%in%rv$params) {
      rollpoints <- graph$rollpoints %>% 
        select(Date, Position, series) %>%
        spread(series, Position) %>%
        arrange(Date)
      dyEpl <- dygraph(rollpoints, main = NULL, group="series", height = "150px") %>% 
        dyOptions(connectSeparatedPoints=TRUE, stepPlot=TRUE, retainDateWindow = TRUE)
      dyEpl %>% dyLegend(show="follow")
    } else {
      NULL
    }
  })
  
  output$graphPanel <- renderUI({
    rv$series <- seriesData()
    if (!is.null(rv$series)){
      if ("positions" %in% rv$params) {
        dyPos <- dygraphOutput(ns("dygraph2"), height = "150px")
      } else {
        dyPos <- NULL
      }
    tagList(
      uiOutput(ns("options")),
      dygraphOutput(ns("dygraph1")),
      dyPos)
        # fluidRow(
        #   column(width = 4, offset=1, style="padding:15px",
        #          checkboxInput(ns("dyLog"), "Log scale", rv$params$log),
        #          checkboxInput(ns("rollPoints"), "Display roll points", rv$params$rollpoints),
        #          checkboxInput(ns("fixStart"), "Base 100", rv$params$scaled),
        #          checkboxInput(ns("positions"), "Positions", rv$params$positions)),
        #   column(width = 3, style="padding:15px",
        #          sliderInput(ns("dyRoller"), "Moving average", 1, 50, rv$params$roller)),
        #   column(width = 3, style="padding:15px",
        #          downloadButton(ns("download"), "CSV"))
    
    } else {
      tags$p(alt)
    }
  })
  
  # options panel
  output$options <- renderUI({
    tagList(
      checkboxGroupInput(ns("displayOptions"), label = NULL,
                         choices = c("Log scale"="log",
                                     "Display roll points"="rollPoints",
                                     "Base 100"="fixStart",
                                     "Show positions"="positions",
                                     "More options..."="moreOptions"),
                         selected = rv$params, inline = TRUE),
      uiOutput(ns("moreOptions"))
    )})
  observe({ rv$params <- input$displayOptions })
  observe({ rv$roller <- input$dyRoller })
  observe({
    if ("moreOptions" %in% rv$params) {
      rv$moreOptions <- "less"
    } else {
      rv$moreOptions <- "more"
    }
  })
  output$moreOptions <- renderUI({
    if (rv$moreOptions == "less") {
      fluidRow(column(width = 3, style="padding:15px",
                      sliderInput(ns("dyRoller"), "Moving average", 1, 50, rv$roller)),
               column(width = 3, style="padding:15px",
                      downloadButton(ns("download"), "CSV")))
    } else {
      NULL
    }
  })
  
  
  download <- function(file) {
    graph <- rv$series
    if (!is.null(graph)){
      d <- graph$series
      if (!is.null(rv$range)) {
        d %<>% filter(Date>=rv$range[1] & Date<=rv$range[2])
      }
      d %>% 
        mutate(Date=strftime(Date, format="%d/%m/%Y")) %>%
        write.table(file, sep=";", dec=",", row.names = FALSE)
    }
  }
  output$download <- downloadHandler("series.csv", download)
  
  reactive(rv$range)
  
}