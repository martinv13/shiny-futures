
## Strategies Tab UI

strategiesTabUI <- function (id, label = "Strategies") {

  ns <- NS(id)
  
  tabPanel(label,
           sidebarLayout(
             sidebarPanel(width=3,
                          tags$h4("Strategies"),
                          selectInput(ns("genericCode"), "Contract code", choices=list()),
                          selectInput(ns("strat"), "Strategy", choices=list()),
                          conditionalPanel(TRUE,
                                           selectInput(ns("window"), "Trend window", choices=list())),
                          selectInput(ns("contractPos"), "Contract #", choices=list()),
                          textInput(ns("leverage"), "Leverage")
                           
             ),
             
             mainPanel(

             )
           )
  )
}