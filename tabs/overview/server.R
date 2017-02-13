## Overview Tab Server

overviewTab <- function (input, output, session) {
  
  ns <- session$ns
  
  futuresData <- FuturesData$new()$singleton
  
  rv <- reactiveValues(codes=NULL, correl=NULL, lastRange=NULL)
  
  observe({
    rv$codes <- futuresData$tickersReactive() %>%
      select( start, Type, `IB Ticker`, RefNum) %>%
      filter(!is.na(RefNum) & RefNum != "NA" & RefNum != "")
    rv$correl <- NULL
    rv$lastRange <- NULL
  })

  output$codesList <- renderRHandsontable({
    dat <- rv$codes
    if (!is.null(dat)) {
      rtable <- rhandsontable(dat, readOnly = FALSE) %>%
        hot_table(contextMenu = FALSE) %>%
        hot_cols(columnSorting = TRUE) %>%
        hot_col(1:dim(dat)[2], readOnly = TRUE) %>%
        hot_col("RefNum", format="0")
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
        hot_col(c("StdDev","1/V"), format="0%")
      if ("1/V Class" %in% colnames(dat)) rtable %<>% 
        hot_col("1/V Class", format="0%")
      if ("ERC" %in% colnames(dat)) rtable %<>% 
        hot_col("ERC", format="0%")
      
      rtable
    }
  })
  
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