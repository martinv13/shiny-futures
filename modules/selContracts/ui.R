
## selContracts Tab UI

selContractsInput <- function (id ) {
  
  ns <- NS(id)
  
  tagList(
    tags$h4(textOutput(ns("titleAction"))),
    
    selectInput(ns("genericCode"), "Contract code", choices=c("...")),
    
    radioButtons(ns("contractType"), 
                 "Type", 
                 c("Continuous roll"="roll", "Individual contract"="single")),
    
    conditionalPanel(paste0("input['", ns("contractType"), "'] == 'single'"),
                     selectInput(ns("completeCode"), "Contract name", choices=c("..."))),
    
    conditionalPanel(paste0("input['", ns("contractType"), "'] == 'roll'"),
                     selectInput(ns("contractNumber"), "Contract #", 1:5, selected = 1),
                     selectInput(ns("rollDate"), "Roll date", 
                                 c("Expiry date" = -1, 
                                   "No roll - price only" = 0, 
                                   "Contract with max OI" = -2, 
                                   1:28)),
                     conditionalPanel(paste0("input['", ns("rollDate"), "'] > 0"),
                        selectInput(ns("strat"), "Strategy", 
                                    Strategy$new()$list),
                        uiOutput(ns("stratOptions"))),
                   conditionalPanel(paste0("input['", ns("rollDate"), "'] != 0"),
                                        textInput(ns("lev"), "Daily leverage", 1))
    ),
    
    uiOutput(ns("actionButtons")),
    
    uiOutput(ns("selContractsList"))
  )
}