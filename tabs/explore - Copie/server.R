
## Explore Tab Server

exploreTab <- function (input, output, session) {
  
  ns <- session$ns
  
  selContractsList <- callModule(selContracts, "selContractsList")
  
  # plot contracts data
  series <- reactive({
    sel <- selContractsList()
    checked <- unlist(child.up(sel, "checked"))
    if (length(checked)>0) {
      checked <- (1:length(checked))[checked]
      labels <- unlist(child.up(sel, "label"))[checked]
      displayedData <- child.up(sel, "series")[checked] %>%
        set_names(labels) %>%
        bind_rows(.id="Series")
      series <- displayedData %>%
        select(Date, Series, Value) %>%
        spread(Series, Value) %>%
        arrange(Date)
      rollpoints <- child.up(sel, "rollpoints")[checked] %>%
        set_names(labels) %>%
        bind_rows(.id="Series")
      list(series=series, rollpoints=rollpoints)
    } else {
      NULL
    }
  })
  
  callModule(plotSeries, "graphPanel", series)
  
  bandWidth <- reactive({
    max(0.01,input$densitySlider)
  })
  
  sampled <- rnorm(1000)
    
  output$densityPlot <- renderDygraph({
    d <- density(sampled, bw=bandWidth())
    dygraph(data.table(d$x, d$y), main="density")
  })
  
  
  observe({
    print(as.POSIXct(input$graphClicked/1000, origin="1970-01-01"))
    print("hello")
  })
  
}

  