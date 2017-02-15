
ui <- fluidPage(
  fluidRow(
    column(width = 4,
           rHandsontableOutput("strats")),
    column(width = 4,
           rHandsontableOutput("assets")),
    column(width = 4,
           rHandsontableOutput("classes"))
  )
)

server <- function (input, output, session) {

  session$onSessionEnded(stopApp)
  
  p <- Portfolios$new("testPortfolio")
  
  values <- reactiveValues(strats=p$strategies, assets=p$assets, classes=p$assetsClasses)
  
  output$strats <- renderRHandsontable({
    rhandsontable(values$strats)
  })
  observe({
    if (!is.null(input$strats)) {
      p$strategies <- hot_to_r(input$strats)
      values$strats <- p$strategies
      values$assets <- p$assets
      values$classes <- p$assetsClasses
    }
  })
  
  output$assets <- renderRHandsontable({
    rhandsontable(values$assets)
  })
  observe({
    if (!is.null(input$assets)) {
      p$assets <- hot_to_r(input$assets)
      values$strats <- p$strategies
      values$assets <- p$assets
      values$classes <- p$assetsClasses
    }
  })
  
  output$classes <- renderRHandsontable({
    rhandsontable(values$classes)
  })
  observe({
    if (!is.null(input$classes)) {
      p$assetsClasses <- hot_to_r(input$classes)
      values$strats <- p$strategies
      values$assets <- p$assets
      values$classes <- p$assetsClasses
    }
  })
    
}

shinyApp(ui, server)
