
## Portfolio Tab UI

portfolioInput <- function ( id , p = list()) {
  
  ns <- NS(id)
  
  tags$div(
    br(),
    fluidRow(
      column(width = 3,
             actionButton(ns("doBacktest"), "Backtest")),
      column(width = 9,
             actionLink(ns("rename"), "Rename"), tags$span(" - "),
             actionLink(ns("duplicate"), "Duplicate"), tags$span(" - "),
             actionLink(ns("delete"), "Delete"),
             checkboxGroupInput(ns("options"), NULL,
                                c("Show" = "show", "Daily returns" = "daily"),
                                selected = c("show", "daily")[c(p$show, p$daily)],
                                inline=TRUE))),
    hr(),
    tabsetPanel(type="pills",
      tabPanel("Strategies", style="min-height:55vh",
               br(),
               rHandsontableOutput(ns("strats"))),
      tabPanel("Assets", style="min-height:55vh",
               br(),
               rHandsontableOutput(ns("assets"))),
      tabPanel("Classes", style="min-height:55vh",
               br(),
#               textInput(ns("leverage"), "Leverage", 1),
               rHandsontableOutput(ns("classes"))),
      tabPanel("Parameters", style="min-height:55vh",
               selectInput(ns("rollDate"), "Rebalance date", c(1:31), p$rollDate),
               checkboxInput(ns("roundedPositions"), "Round positions sizes"),
               conditionalPanel(paste0("input['",ns("roundedPositions"),"']==true"), 
                                textInput(ns("notional"), "Notional size (kEUR)", 100),
                                textInput(ns("minRound"), "Minimum position rounding up", 0.6)),
               checkboxInput(ns("adaptLeverage"), "Adapt leverage (normalize VaR)"),
               conditionalPanel(paste0("input['",ns("adaptLeverage"),"']==false"), 
                                textInput(ns("leverage"), "Fixed leverage", 1)),
               conditionalPanel(paste0("input['",ns("adaptLeverage"),"']==true"), 
                                textInput(ns("quantileVaR"), "Quantile", .95),
                                textInput(ns("VaR"), "Value at Risk (%)", 3)))))
  
}