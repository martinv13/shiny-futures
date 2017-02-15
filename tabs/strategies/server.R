
## Strategies Tab Server

strategiesTab <- function (input, output, session) {
  
  ns <- session$ns
  
  updateSelectInput(session, ns("strat"), choices = names(strategies))
  

}

  