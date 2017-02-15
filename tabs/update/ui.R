
## Update Tab UI

updateTabUI <- function (id, label = "Update") {
  
  ns <- NS(id)
  
  tabPanel(label,
           sidebarLayout(
             sidebarPanel( width = 3,
               tags$h4("Update data from Quandl"),
               uiOutput(ns("updateButton")),
               textOutput(ns("updateStatus")),
               br(),
               tags$h4("Connection settings"),
               textInput(ns(("quandlKey")), "Quandl API key", value=db$readKey("quandl_api_key","")),
               checkboxInput(ns(("withProxy")), "Configure proxy", value=db$readKey("withProxy", FALSE)),
               conditionalPanel(paste0("input['",ns("withProxy"),"'] === true"),
                                textInput(ns("proxyAddress"), "Proxy address", value=db$readKey("proxyAddress","")),
                                numericInput(ns("proxyPort"), "Proxy port", value=db$readKey("proxyPort", 0)),
                                textInput(ns("proxyUsername"), "Username", value=db$readKey("proxyUsername","")),
                                passwordInput(ns("proxyPassword"), "Password", value=db$readKey("proxyPassword","")))
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Future contracts",
                          br(),
                          rHandsontableOutput(ns("tickersList"))
                 ),
                 tabPanel("Rates",
                          br(),
                          uiOutput(ns("ratesSeriesUI")),
                          rHandsontableOutput(ns("ratesList"))
                 ),
                 tabPanel("FX",
                          br(),
                          uiOutput(ns("fxSeriesUI")),
                          rHandsontableOutput(ns("fxList"))
                 )
               ),
               br(),br(),br()
             )
           )
  )
  
}