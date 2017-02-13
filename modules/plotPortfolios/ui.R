
# plotPortfolios UI

plotPortfoliosUI <- function (id) {
  
  ns <- NS(id)
  
  uiOutput(ns("graphPanel"))
    
}