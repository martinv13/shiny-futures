
## Update Tab Server

updateTab <- function (input, output, session) {
  
  ns <- session$ns
  
  ratesData$load()
  fXData$load()
  
  values <- reactiveValues(qf=NULL, fetching=FALSE, status="Ready.", 
                           button="Update all", ratesSeries=ratesData$data,
                           fxSeries=fXData$data)
  
  output$tickersList <- renderRHandsontable({
    DT <- NULL
    if (!is.null(input$tickersList)) {
      DT <- setDT(hot_to_r(input$tickersList))
      futuresData$tickers <- DT
      DT <- futuresData$tickers
    } else {
      DT <- futuresData$tickersReactive()
    }
    if (!is.null(DT)) {
      rhandsontable(DT, readOnly = FALSE) %>%
        hot_cols(columnSorting = TRUE) %>%
        hot_col(col="nbContracts", readOnly = TRUE) %>%
        hot_col(col="lastFetch", readOnly = TRUE) %>%
        hot_col(col="lastPrice", readOnly = TRUE, format = "0.00") %>%
        hot_col(col="start", readOnly = TRUE) %>%
        hot_col("sizeEUR", readOnly = TRUE, format="0,0") %>%
        hot_col("Margin", format="0%") %>%
        hot_col("RefNum", format="0")
    }
  })
  
  params <- reactive({
    p <- list( quandl_api_key = input$quandlKey)
    if (input$withProxy) {
      p$proxy = list(proxy = input$proxyAddress,
                     proxyport = input$proxyPort,
                     proxyusername = input$proxyUsername,
                     proxypassword = input$proxyPassword)  
    } else {
      p$proxy = list()
    }
    p
  })
  
  observe(db$writeKey("withProxy", input$withProxy))
  observe(db$writeKey("proxyAddress", input$proxyAddress))
  observe(db$writeKey("proxyUsername", input$proxyUsername))
  observe(db$writeKey("proxyPort", input$proxyPort))
  observe(db$writeKey("proxyPassword", input$proxyPassword))
  observe(db$writeKey("quandl_api_key", input$quandlKey))
  
  observe({
    if(values$fetching) {
      isolate({
        if (is.null(values$qf)) {
          values$qf <- QuandlFetcher$new(futuresData$tickers[["Quandl Code"]], 
                                         ratesData$codes,
                                         fXData$codes,
                                         params())
        }
        values$qf$fetchOne()
      })
      complete <- isolate(values$qf$complete())
      if (complete<1) {
        values$status <- paste0(isolate(values$qf$status), " (", round(complete*100), "%)")
        invalidateLater(0, session)
      } else {
        futuresData$load()
        ratesData$load()
        values$ratesSeries <- ratesData$data
        values$status <- isolate(values$qf$status)
        values$fetching <- FALSE
        values$button <- "Update all"
        values$status <- "Done."
      }
    }
  })
  
  output$updateStatus <- renderText(values$status)
  
  output$updateButton <- renderUI(actionButton(ns("updateAll"), values$button))
  
  observeEvent(input$updateAll, {
    values$fetching <- !values$fetching
    values$button <- ifelse(values$fetching, "Stop", "Update all")
    values$status <- ifelse(values$fetching, "Starting...", "Ready.")
    values$qf <- NULL
  })
  
  output$ratesList <- renderRHandsontable({
    DT <- NULL
    if (!is.null(input$ratesList)) {
      DT <- setDT(hot_to_r(input$ratesList))
      ratesData$codes <- DT
      DT <- ratesData$codes
    } else {
      DT <- ratesData$codesReactive()
    }
    if (!is.null(DT)) {
      rhandsontable(DT, readOnly = FALSE) %>%
        hot_cols(columnSorting = TRUE) %>%
        hot_col(col="lastFetch", readOnly = TRUE)
    }
  })
  
  output$fxList <- renderRHandsontable({
    DT <- NULL
    if (!is.null(input$fxList)) {
      DT <- setDT(hot_to_r(input$fxList))
      fXData$codes <- DT
      DT <- fXData$codes
    } else {
      DT <- fXData$codesReactive()
    }
    if (!is.null(DT)) {
      rhandsontable(DT, readOnly = FALSE) %>%
        hot_cols(columnSorting = TRUE) %>%
        hot_col(col="lastFetch", readOnly = TRUE)
    }
  })
  
  output$ratesSeriesUI <- renderUI({
    if (!is.null(values$ratesSeries)) {
      tagList(
        dygraphOutput(ns("ratesSeriesGraph")),
        selectInput(ns("selRates"), "Select series", unique(values$ratesSeries$shortName), multiple = TRUE)
      )
    }
  })
  
  output$ratesSeriesGraph <- renderDygraph({
    temp <- values$ratesSeries %>%
      filter(shortName %in% input$selRates)
    if (dim(temp)[1]>0) {
      temp %>%
        spread(shortName, Value) %>%
        dygraph() %>%
        dyRangeSelector()
    }
  })
  
  output$fxSeriesUI <- renderUI({
    if (!is.null(values$fxSeries)) {
      tagList(
        dygraphOutput(ns("fxSeriesGraph")),
        selectInput(ns("selFX"), "Select series", unique(values$fxSeries$shortName), multiple = TRUE)
      )
    }
  })
  
  output$fxSeriesGraph <- renderDygraph({
    if(!is.null(values$fxSeries)){
      temp <- values$fxSeries %>%
        filter(shortName %in% input$selFX)
      if (dim(temp)[1]>0) {
        temp %>%
          spread(shortName, Value) %>%
          dygraph() %>%
          dyRangeSelector()
      }
    }
  })
  
}