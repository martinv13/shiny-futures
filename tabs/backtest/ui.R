
## Backtest Tab UI

backtestTabUI <- function (id, label = "Backtest") {
  
  ns <- NS(id)
  tabPanel(label,
           fluidRow(
             column(width=5,
                    uiOutput(ns("portfolios")),
                    tags$label("Save / Export"),br(),
                    actionButton(ns("save"), "Save all"),
                    downloadButton(ns("export"), "Export"),
                    fileInput(ns("import"), "Import", accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")),
                    br(),br()
             ),
             column(width=7,
                    tabsetPanel(
                      tabPanel("Returns",
                               plotPortfoliosUI(ns("seriesPlot"))),
                      tabPanel("Statistics",
                               br(),
                               performanceStatsUI(ns("portfoliosStats"))),
                      tabPanel("Positions",
                               sliderInput(ns("posDateRange"),NULL,
                                           min=as.Date("1970-01-01"),
                                           max=as.Date(today()),
                                           value = c(as.Date("2016-01-01"), as.Date(today())),
                                           width="500px"),
                               tags$div(style = "max-height:82vh;overflow-y:auto;",
                                 rHandsontableOutput(ns("positions"))))))
           )
  )
  
}