tempStat <- NULL
portfolioPos <- NULL
## Backtest Tab Server

backtestTab <- function (input, output, session) {
  
  ns <- session$ns
  
  pCounter <- 1
  
  portfolios <- Portfolios$new()
  
  portfolios$load()
  
  rv <- reactiveValues(
    list=portfolios$list, 
    series=portfolios$series,
    positions=portfolios$positions,
    posRange=c(as.Date("2016-01-01"), as.Date(today())))

  afterUpdate <- function(){
    rv$series <- portfolios$series
    rv$positions <- portfolios$positions
  }
  
  afterUpdateList <- function(){
    print("list updated")
    rv$list <- NULL
    rv$list <- portfolios$list
    rv$series <- portfolios$series
    rv$positions <- portfolios$positions
  }
  
  output$portfolios <- renderUI({
    portfoliosList <- rv$list
    selected <- NULL
    tabs <- c(
      lapply(seq_along(portfoliosList), function(i){
        p <- portfoliosList[[i]]
        name <- paste0("portfolio-", pCounter)
        pCounter <<- pCounter+1
        if (p$selected) {
          selected <<- p$name
        }
        callModule(portfolio, name, p, portfolios, afterUpdate, afterUpdateList)
        tabPanel(p$name, 
                 portfolioInput(ns(name), p))
      }),
      list(tabPanel("+ New", style="min-height:75vh",
                    br(),
                    textInput(ns("newPortfolioName"), "New portfolio name", "portfolio"),
                    selectInput(ns("createFrom"), 
                                "From existing portfolios",
                                unname(unlist(child.up(portfoliosList, "name"))),
                                multiple = TRUE),
                    actionButton(ns("newPortfolio"), "Create")))
    )
    do.call( tabsetPanel, c(tabs, "selected"=selected, id=ns("tabsetPortfolios")))
  })
  
  observeEvent(input$tabsetPortfolios, {
    for (p in rv$list) {
      p$selected <- input$tabsetPortfolios == p$name
    }
  })
  
  selRange <- callModule(plotPortfolios,
                           "seriesPlot",
                           reactive({
                             list(
                               series=rv$series,
                               positions=rv$positions)}))
  
  observe({
    if (!is.null(rv$positions)) {
      portfolioPos <<- rv$positions
    }
  })
  
  output$positions <- renderRHandsontable({
    if (!is.null(rv$positions)) {
      rhandsontable(rv$positions %>% 
                      filter( Date >= rv$posRange[1] & Date <= rv$posRange[2]) %>%
                      arrange(desc(Date), genericCode)) %>%
        hot_cols(columnSorting = TRUE,
                 colWidths = 110*c(1,1,.8,1,1,.8,.8,.8),
                 manualColumnResize = TRUE) %>%
        hot_col(1:8, readOnly = TRUE ) %>%
        hot_col("period", format="0")
        
    }
  })
  
  observe({
    rv$posRange <- input$posDateRange
  })
  
  callModule(performanceStats, "portfoliosStats", reactive({rv$series}), selRange)

  observeEvent(input$newPortfolio, {
    if (length(input$createFrom)>1) {
      portfolios$merge(input$newPortfolioName, input$createFrom)
    } else {
      portfolios$add(Portfolio$new(input$newPortfolioName))
    }
    afterUpdateList()
  })
  
  output$export <- downloadHandler("portfolios.csv", portfolios$export)
  
  observeEvent(input$import, {
    inpf <- input$import
    if (!is.null(inpf)){
      portfolios$import(inpf$datapath)
      portfolios$backtestAll()
      rv$list <- portfolios$list
      rv$series <- portfolios$series
    }
  })
  
  observeEvent(input$save, {
    portfolios$save()
  })

}