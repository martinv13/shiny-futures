
# plotPortfolios module server

plotPortfolios <- function (input, output, session, seriesData, alt="No portfolio to display") {

  ns <- session$ns
  
  values <- reactiveValues(seriesData=NULL, 
                           range=NULL,
                           rangeScale=NULL,
                           params=list(scaled=FALSE, 
                                       log=TRUE))
  
  observe({
    values$params <- list(log="dyLog" %in% input$displayOptions,
                          scaled="fixStart" %in% input$displayOptions)
  })
  
  debouncedRange <- debounce({input$dySeries_date_window}, 600)
  
  observe({
    ra <- debouncedRange()
    if (!is.null(ra) && length(ra)==2) {
      range <- as.POSIXct(ra)
      if (diff(range)>0) {
        values$range <- range + 24*3600
        if (!is.null(values$params$scaled) && values$params$scaled) {
          values$rangeScale <- values$range[1]
        }
      }
    }
  })
  
  output$dySeries <- renderDygraph({
    graph <- values$seriesData
    params <- values$params
    dy <- NULL 
    if (!is.null(graph)) {
      graphseries <- graph$series %>% arrange(Date)
      cols <- graphseries %>% select(-Date, -period)
      series <- xts(cols, 
                    order.by = graphseries$Date)
      if (params$scaled) {
        scales <- unlist(lapply(graphseries %>%
                           filter(Date >= as.Date(values$rangeScale)) %>%
                           select(-Date, -period),
                           function(x) x[match(TRUE, !is.na(x))[1]]))/100
        series %<>% scale(center=rep(0, length(scales)), scale=scales)
      }
      range <- isolate(values$range)
      dyEpl <- dygraph(series, main = NULL, group="series") %>% 
        dyRangeSelector(dateWindow = range) %>% 
        dyOptions(logscale=params$log, connectSeparatedPoints=TRUE, retainDateWindow = TRUE) %>%
        dyCallbacks(clickCallback="dygraphClick") %>% 
        dyLegend(show="follow")
    } else {
      NULL
    }
  })
  
  output$dyPositions <- renderDygraph({
    if (!is.null(values$seriesData) && !is.null(values$seriesData$positions)) {
      pos <- values$seriesData$positions %>% 
        group_by(period) %>%
        mutate(Margin = sum(abs(Margin*netPosition)),
               timeToExp = maxDate - Date)
      pos$displayPos <- pos[[input$positionsValue]]
      selPos <- pos %>% 
        unite(selCode, Code, Portfolio, sep="-") %>%
        select(Date, selCode, displayPos) %>%
        bind_rows(pos %>% 
                    distinct(Date, Portfolio, .keep_all=TRUE) %>%
                    mutate(selCode = paste("Leverage",Portfolio,sep="-"),
                           displayPos=lev) %>%
                    select(Date, selCode, displayPos)) %>%
        bind_rows(pos %>% 
                    distinct(Date, Portfolio, .keep_all=TRUE) %>%
                    mutate(selCode = paste("Req.margin",Portfolio,sep="-"),
                           displayPos=Margin) %>%
                    select(Date, selCode, displayPos))
      
      if (length(input$positionsSel)==0) {
        range <- isolate(values$range)
        if (is.null(range)) {
          range <- c(min(values$seriesData$series$Date),max(values$seriesData$series$Date))
        }
        selPos <- data.table(Date=range,val=c(0,0))
      } else {
        selPos %<>%
          filter(selCode %in% input$positionsSel) %>%
          select(Date, selCode, displayPos) %>%
          spread(selCode, displayPos)
      }
      selPos %>%
          dygraph(main = NULL, group="series", height = "150px") %>% 
          dyOptions(connectSeparatedPoints=TRUE, stepPlot=TRUE, retainDateWindow = TRUE ) %>%
          dyLegend(show="follow")
    }
  })
  
  output$graphPanel <- renderUI({
    values$seriesData <- seriesData()
    if (!is.null(values$seriesData) && !is.null(values$seriesData$series)){
      pos <- values$seriesData$positions
      portL <- (pos %>% distinct(Portfolio))$Portfolio
      posChoices <- portL %>% 
        lapply(function(p){
          codes <- c("Leverage", "Req.margin",
                     (pos %>% filter(Portfolio==p) %>% distinct(Code))$Code)
          as.list(paste(codes, p, sep="-"))
        }) %>% setNames(portL)
      tagList(
        checkboxGroupInput(ns("displayOptions"), 
                          label = NULL, 
                          choices = c("Log scale" = "dyLog",
                                      "Base 100" = "fixStart"),
                          selected = "dyLog",
                          inline = TRUE),
        dygraphOutput(ns("dySeries")),
        dygraphOutput(ns("dyPositions"), height = "150px"),
        fluidRow(
          column(width=4,
                 selectInput(ns("positionsSel"), "Show positions",
                             choices = posChoices,
                             multiple = TRUE)),
          column(width=4,
                 selectInput(ns("positionsValue"), "type",
                             choices = c("Net position"="netPosition",
                                         "Weight"="Weight",
                                         "Position"="Position",
                                         "Time to exp."="timeToExp")))
        )
      )
    } else {
      fluidRow(
        tags$p(alt)
      ) 
    }
  })
  
  observe({
    print(input$primarySel)
  })
  
  
  reactive(values$range)
}