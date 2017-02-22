
## Portfolio Tab Server

portfolio <- function (input, output, session, p, portfolios, afterUpdate, afterUpdateList) {
  
  ns <- session$ns
  
  values <- reactiveValues(strats=p$strategies, assets=p$assets, classes=p$assetsClasses)
  
  observe({
    p$show <- "show" %in% input$options
    afterUpdate()
  })
  
  observeEvent(input$doBacktest, {
    pr <- Progress$new(session)
    pr$set(message=paste("Backtesting",p$name))
    p$backtest(function(det=NULL, val=0){
      pr$set(detail=det, value=val)
    })
    pr$set(detail=NULL, message="Done", value=1)
    pr$close()
    afterUpdate()
  })

  observe({
    p$toogleDaily("daily" %in% input$options)
  })
  
  output$strats <- renderRHandsontable({
    rhandsontable(values$strats) %>%
      hot_cols(colWidths = 75*c(1,1,2,1,1),
               columnSorting = TRUE) %>%
      hot_col("Total", readOnly = TRUE, format="0%")%>%
      hot_col("Weight", format="0[.]0")
  })
  initStrat <- FALSE
  observe({
    if (!is.null(input$strats)) {
      if (initStrat) {
        newStrats <- hot_to_r(input$strats)
        p$strategies <- newStrats
        newStrats[is.na(Strategy),Strategy:="long"]
        newStrats[is.na(Weight),Weight:=1]
        values$strats <- p$strategies[newStrats[,.(Code, Strategy, Weight)],
                                      on=c("Code","Strategy","Weight"),
                                      mult="first"]
        values$assets <- p$assets
        values$classes <- p$assetsClasses
      } else {
        initStrat <<- TRUE
      }
    }
  })
  
  output$assets <- renderRHandsontable({
    rhandsontable(values$assets) %>%
      hot_cols(colWidths = 75*c(1,1,1.5,1,1),
               columnSorting = TRUE) %>%
      hot_col("Contract #", format="0") %>%
      hot_col("Weight", format="0[.]0") %>%
      hot_col("Total", readOnly = TRUE, format="0%")
  })
  initAssets <- FALSE
  observe({
    if (!is.null(input$assets)) {
      if (initAssets) {
        p$assets <- hot_to_r(input$assets)
        values$strats <- p$strategies
        values$assets <- p$assets
        values$classes <- p$assetsClasses
      } else {
        initAssets <<- TRUE
      }
    }
  })
  
  output$classes <- renderRHandsontable({
    rhandsontable(values$classes) %>%
      hot_cols(colWidths = 75*c(2,2),
               columnSorting = TRUE) %>%
      hot_col("Weight", format="0%")
  })
  initClasses <- FALSE
  observe({
    if (!is.null(input$classes)) {
      if (initClasses) {
        p$assetsClasses <- hot_to_r(input$classes)
        values$strats <- p$strategies
        values$assets <- p$assets
        values$classes <- p$assetsClasses
      } else {
        initClasses <<- TRUE
      }
    }
  })
  
  modalName <- function(type="rename") {
    modalDialog(
      textInput(ns("portfolioName"), "Enter a new name", placeholder = "Portfolio"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns(ifelse(type=="rename","okRename","okDuplicate")), "OK")
      )
    )
  }
  
  observeEvent(input$duplicate, {
    showModal(modalName("duplicate"))
  })
  
  observeEvent(input$rename,{
    showModal(modalName("rename"))
  })
  
  observeEvent(input$okRename, {
    removeModal()
    if (!is.null(input$portfolioName) && nchar(input$portfolioName)>1) {
      p$name <- input$portfolioName
      portfolios$uniquify()
      afterUpdateList()
    }
  })
  
  observeEvent(input$okDuplicate, {
    removeModal()
    if (!is.null(input$portfolioName) && nchar(input$portfolioName)>1){
      p2 <- p$clone()
      p2$name <- input$portfolioName
      portfolios$add(p2)
      afterUpdateList()
    }
  })
  
  observeEvent(input$delete, {
    showModal(modalDialog(
      span(paste("Please confirm deletion of", p$name)),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("okDelete"), "OK")
      )
    ))
  })
  
  observeEvent(input$okDelete, {
    removeModal()
    portfolios$delete(p$name)
    afterUpdateList()
  })
  
  observe({
    p$rounded <- input$roundedPositions
    p$notional <- as.numeric(input$notional)
    p$minRound <- as.numeric(input$minRound)
  })
  observe({
    p$adaptLeverage <- input$adaptLeverage
    p$leverage <- as.numeric(input$leverage)
    p$quantileVaR <- as.numeric(input$quantileVaR)
    p$VaR <- as.numeric(input$VaR)/100
  })
  observe({
    p$rollDate <- as.numeric(input$rollDate)
  })
  
 
}