
## Explore Tab UI

exploreTabUI <- function (id, label = "Explore") {

  ns <- NS(id)
  
  jscode <- paste0('
  function dygraphClick(e,x){
    console.log("click");
    Shiny.onInputChange("',ns("graphClicked"),'", x);
  }')
  
  tabPanel(label,
           tags$head(tags$script(HTML(jscode))),
           sidebarLayout(
             sidebarPanel( width=3,
                selContractsInput(ns("selContractsList")),
                br(),
                actionButton(ns("clearStratsCache"), "Clear strat. cache")
             ),
             
             mainPanel(
              plotSeriesUI(ns("graphPanel")),
              hr(),
              performanceStatsUI(ns("perfStats")),
              fluidRow(
                 column(width = 6
                 ),
                 column(width = 6
                 )
               )
             )
           )
  )
}