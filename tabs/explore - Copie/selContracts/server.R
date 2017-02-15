
## selContracts Server

selContracts <- function (input, output, session) {
  
  ns <- session$ns
  
  # Selected contracts list
  selContractsList <- reactiveValues("contracts"=list(),
                                     "selected"=0)
  
  # contract selector
  observe({
    updateSelectInput(session, "genericCode",
                      choices = do.call(c, ((futuresData$tickersReactive() %>%
                                               group_by(Type) %>%
                                               do(vals=data.frame(.)))$vals %>%
                                              lapply(function(x){
                                                temp <- list()
                                                temp[[x$Type[1]]] <- x$Quandl.Code
                                                names(temp[[x$Type[1]]]) <- paste(x$IB.Ticker, "-", x$Name)
                                                return(temp)
                                              }))))
    
  })

  individualContracts <- reactive({
    if(is.data.frame(futuresData$data)){
      as.list(futuresData$data %>% ungroup() %>% filter(genericCode == input$genericCode) %>%
                distinct(completeCode, .keep_all = TRUE) %>% 
                arrange(desc(expDate)) %>%
                select(completeCode))$completeCode
    } else {
      list()
    }
  })
  
  output$stratOptions <- renderUI({
    if (!is.null(input$strat) && !is.null(strategies[[input$strat]])) {
      inputs <- list()
      argss <- names(formals(strategies[[input$strat]]))[-(1:2)]
      if (length(argss)>0) {
        for(i in 1:length(argss)) {
          inputs <- c(inputs, textInput(ns(paste0("stratOption-",i)), argss[i]))
        }
        return (tagList(inputs))
      }
    }
  })
  
  observe({
    updateSelectInput(session, "completeCode", choices = individualContracts())
  })
  
  # update input when selecting new item
  updateInputs <- function(params) {
    if (!is.null(params)) {
      updateRadioButtons(session, "contractType", selected = params$contractType)
      updateSelectInput(session, "genericCode", selected = params$genericCode)
      updateSelectInput(session, "completeCode", selected = params$completeCode)
      updateSelectInput(session, "rollContract", selected = params$rollContract)
      updateSelectInput(session, "rollDate", selected = params$rollDate)
      updateSelectInput(session, "lev", selected = params$leverage)
    }
  }
  
  getParams <- reactive({
    list(contractType = input$contractType,
         genericCode = input$genericCode,
         rollDate = input$rollDate,
         rollContract = input$rollContract,
         completeCode = input$completeCode,
         leverage = input$lev)
  })
  
  # "add" button
  observeEvent(input$addContract, {
    l <- length(selContractsList$contracts)
    params <- getParams()
    contractData <- getContractData(params)
    labels <- child.up(selContractsList$contracts, "label")
    label <- contractData$label
    if (label %in% labels) {
      i<-1
      while (label %in% labels) {
        label <- paste(label, "-", i)
        i <- i+1
      }
    }
    selContractsList$contracts[[l+1]] <- list(name=input$genericCode,
                                              checked = TRUE,
                                              params=params,
                                              series = contractData$series,
                                              rollpoints = contractData$rollpoints,
                                              label = label)
  })
  
  # "update" button
  observeEvent(input$updateContract, {
    s <- selContractsList$selected
    if (s>0) {
      params <- getParams()
      contractData <- getContractData(params)
      labels <- child.up(selContractsList$contracts, "label")[-s]
      label <- contractData$label
      if (label %in% labels) {
        i<-1
        while (label %in% labels) {
          label <- paste(label, "-", i)
          i <- i+1
        }
      }
      selContractsList$contracts[[s]] <- list(name=input$completeCode,
                                              checked = TRUE,
                                              params=params,
                                              series = contractData$series,
                                              rollpoints = contractData$rollpoints,
                                              label = label)
    }
  })
  
  # "delete" button
  observeEvent(input$deleteContract, {
    l <- length(selContractsList$contracts)
    s <- selContractsList$selected
    if (s>0 && s<=l) {
      selContractsList$contracts <- selContractsList$contracts[-s]
      selContractsList$selected = min(s, l-1)
      if(selContractsList$selected>0) {
        updateInputs(selContractsList$contracts[[selContractsList$selected]]$params)
      }
    }
  })
  
  
  output$selContractsList <- renderUI({
    l <- length(selContractsList$contracts)
    if (l>0) {
      list(
        tags$hr(),
        tags$h4("Selected contracts"),
        lapply(1:l, function(i){
          e <- selContractsList$contracts[[i]]
          ckbxName <- paste0("selContractCkbx_",i)
          checkboxInput(ns(ckbxName), actionLink(ns(paste0(ckbxName,"_lk")), e$label), value=e$checked)
        }))
    }
  })
  
  # observe checkboxes
  lapply(1:20, function(i){
    ckbxName <- paste0("selContractCkbx_",i)
    observeEvent(input[[ckbxName]], {
      selContractsList$contracts[[i]]$checked = input[[ckbxName]]
    })
    observeEvent(input[[paste0(ckbxName,"_lk")]], {
      print(paste0(selContractsList$contracts[[i]]$name," clicked"))
      selContractsList$selected = i
      updateInputs(selContractsList$contracts[[i]]$params)
    })
  })
  
  observe({
    output$titleAction <- renderText(ifelse(selContractsList$selected>0, 
                                             "Update contract", 
                                             "Add contract"))
  })
  
  output$actionButtons <- renderUI({
    if (selContractsList$selected>0) {
      tagList(   actionButton(ns("updateContract"), "Update"),
                 actionButton(ns("deleteContract"), "Delete"),
                 actionButton(ns("addContract"), "Add"))
    } else {
      tagList(   actionButton(ns("addContract"), "Add contract"))
    }
  })
  
  return(reactive({selContractsList$contracts}))
  
}