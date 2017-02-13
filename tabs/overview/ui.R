
## Overview Tab UI

overviewTabUI <- function (id, label = "Overview") {
  
  ns <- NS(id)
  
  tabPanel(label,
             fluidRow(
               column(width=1, 
                      actionButton(ns("compute"), "Compute")),
               column(width=4,
                      sliderInput(ns("dateRange"),NULL,
                                  min=as.Date("1970-01-01"),
                                  max=as.Date(today()),
                                  value = c(as.Date("2000-01-01"), as.Date(today())),
                                  width="500px")),
               column(width=5,
                      tags$div(style="margin-top:10px",
                               textOutput(ns("rangeCalc"), inline="TRUE")))),
             fluidRow(
               column(width=4, rHandsontableOutput(ns("codesList"))),
               column(width=8, plotOutput(ns("correlPlot")))
             )
           )
  
}