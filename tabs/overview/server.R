## Overview Tab Server

overviewTab <- function (input, output, session) {
  
  ns <- session$ns
  
  futuresData <- FuturesData$new()$singleton
  
  rv <- reactiveValues(codes=NULL, correl=NULL, lastRange=NULL)
  
  observe({
    rv$codes <- futuresData$tickersReactive() %>%
      select(`IB Ticker`, start, Type, RefNum, sizeEUR) %>%
      filter(!is.na(RefNum) & RefNum != "NA" & RefNum != "")
    rv$correl <- NULL
    rv$lastRange <- NULL
  })

  output$codesList <- renderRHandsontable({
    dat <- rv$codes
    if (!is.null(dat)) {
      rtable <- rhandsontable(dat, readOnly = FALSE) %>%
        hot_table(contextMenu = FALSE) %>%
        hot_cols(columnSorting = TRUE, fixedColumnsLeft = 1) %>%
        hot_col(1:dim(dat)[2], readOnly = TRUE) %>%
        hot_col("RefNum", format="0") %>%
        hot_col("sizeEUR", format="0,0")
      if (!is.null(isolate(rv$lastRange))) {
        rangeStart <- format(isolate(rv$lastRange)[1], "%Y%m%d")
        rendererText <- paste0("
                  function (instance, td, row, col, prop, value, cellProperties) {
                           Handsontable.renderers.NumericRenderer.apply(this, arguments);
                           if (value>\"",rangeStart,"\") {
                           td.style.background = 'orange';
      }}")
        rtable %<>%
          hot_col("start", renderer=rendererText)
      }
      if ("StdDev" %in% colnames(dat)) rtable %<>% 
        hot_col(c("StdDev","StdDev0","StdDev0Down","MAD",
                  "MAD0","MAD0Down","VaR"), format="0%") %>%
        hot_col(c("1/StdDev","1/StdDev0","1/StdDev0Down","1/MAD",
                  "1/MAD0","1/MAD0Down","1/VaR","ERC"), format="0.00")
      rtable
    }
  })
  
  cols <- c("StdDev","StdDev0","StdDev0Down","MAD",
            "MAD0","MAD0Down","VaR","1/StdDev","1/StdDev0",
            "1/StdDev0Down","1/MAD","1/MAD0","1/MAD0Down","1/VaR","ERC")
  
  output$correlPlot <- renderPlot({
    if (!is.null(rv$correl)) {
      dat <- melt(rv$correl)
      ggplot(dat, aes(Var1, Var2)) +
        geom_tile(aes(fill = value), show.legend = FALSE) + 
        geom_text(aes(label = round(value, 1))) +
        scale_fill_gradientn(colours=c("forestgreen","white","red"),
                             values  = rescale(c(min(dat$value), 0, max(dat$value)))) +
        xlab(NULL) + ylab(NULL) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  },width = 1000, height = 800)
  
  output$weightPlotControls <- renderUI({
    if (!is.null(rv$codes) && "StdDev" %in% colnames(rv$codes)) {
      fluidRow(
        column(width = 6,
               selectInput(ns("selWeightSeries"),
                           "Select series", cols, cols,
                           multiple = TRUE)),
        column(width = 6,
               selectInput(ns("selContracts"),
                           "Select contracts", rv$codes[,`IB Ticker`],rv$codes[,`IB Ticker`],
                           multiple = TRUE))
      )
    }
  })
  
  output$weightPlot <- renderPlot({
    if (!is.null(rv$codes) && "StdDev" %in% colnames(rv$codes)) {
      melt(rv$codes[`IB Ticker` %in% input$selContracts,][,
                     c("IB Ticker",input$selWeightSeries),with=FALSE], 
           id.vars = c("IB Ticker"), mesure.vars=input$selWeightSeries) %>%
      ggplot(aes(y=value, x=`IB Ticker`,fill=variable)) +
        geom_bar(stat="identity", position=position_dodge())
    }
  })
  
  output$dlData <- downloadHandler(
    filename = "weights.csv",
    content = function(file) {
      write.table(isolate(rv$codes), file, sep=";", dec=",", row.names = FALSE)
    }
  )
  
  observeEvent(input$compute, {
    out <- futuresData$correlations(range=input$dateRange)
    rv$lastRange <- input$dateRange
    rv$codes <- out$stdDev
    rv$correl <- out$correl
  })
  
  output$rangeCalc <- renderText({
    if (!is.null(rv$lastRange)) {
      paste("Returns from",
            format(rv$lastRange[1], "%Y-%m-%d"),
            "to",
            format(rv$lastRange[2], "%Y-%m-%d"))
    }
  })
  
}