
## selContracts Server

selContracts <- function (input, output, session) {
  
  ns <- session$ns
  
  fd <- FuturesData$new()$singleton
  
  # Selected contracts list
  selContractsList <- Contracts$new()
  
  values <- reactiveValues(
    labels=list(),
    params=list(),
    series=NULL,
    selectID=0)
  
  # available generic codes
  observe({
    updateSelectInput(session, "genericCode",
                      choices = fd$genericCodesReactive())
  })

  # available complete codes
  observe({
    updateSelectInput(session, "completeCode", 
                      choices = fd$completeCodesReactive(input$genericCode)())
  })
  
  # render strategy parameters inputs
  output$stratOptions <- renderUI({
    if (!is.null(input$strat)) {
      if (!is.null(values$params) && !is.null(values$params$strategy) && input$strat==values$params$strategy) {
        stratArgs <- values$params$stratParams
      } else {
        stratArgs <- Strategy$new(input$strat)$args
      }
      fields <- lapply(seq_along(stratArgs), function(i){
        textInput(ns(paste0("stratOption-",i)), 
                  names(stratArgs)[i], 
                  stratArgs[[i]])
      })
      if (length(fields)>0) {
        inputPanel(fields)
      } else {
        list()
      } 
    }
  })
  
  # update input when selecting new item
  updateParams <- function () {
    params <- selContractsList$params
    if (!is.null(params)) {
      updateRadioButtons(session, "contractType", selected = params$contractType)
      updateSelectInput(session, "genericCode", selected = params$genericCode)
      updateSelectInput(session, "completeCode", selected = params$completeCode)
      updateSelectInput(session, "contractNumber", selected = params$contractNumber)
      updateSelectInput(session, "rollDate", selected = params$rollDate)
      updateSelectInput(session, "lev", selected = params$leverage)
      updateSelectInput(session, "strat", selected = params$strategy)
      lapply(seq_along(params$stratParams), function(i){
        pname <- names(params$stratParams)[i]
        updateTextInput(session, 
                        ns(paste0("stratOption-",i)),
                        label=pname,
                        value=as.character(params$stratParams[[pname]]))
      })
      values$params <- params
    }
  }

  getParams <- reactive({
    list(contractType = input$contractType,
         genericCode = input$genericCode,
         rollDate = input$rollDate,
         contractNumber = input$contractNumber,
         completeCode = input$completeCode,
         leverage = input$lev,
         strategy = input$strat,
         stratParams = lapply(1:10, function(i){
           input[[paste0("stratOption-",i)]]
         }) %>% remove.nulls())
  })
  
  updateAll <- function(){
    values$labels <- selContractsList$labels
    values$series <- selContractsList$series
    values$selectID <- selContractsList$selectID
  }
  
  # "add" button
  observeEvent(input$addContract, {
    withProgress({
      params <- getParams()
      incProgress(1/3)
      selContractsList$add(Contract$new(params))
      incProgress(2/3)
      updateAll()
    }, message="Working...")
  })
  
  # "update" button
  observeEvent(input$updateContract, {
    withProgress({
      params <- getParams()
      incProgress(1/3)
      selContractsList$update(Contract$new(params))
      incProgress(2/3)
      updateAll()
    }, message="Working...")
  })

  # "delete" button
  observeEvent(input$deleteContract, {
    selContractsList$delete()
    updateAll()
  })
  
  # "apply code to all" link
  observeEvent(input$applyCode, {
    params <- getParams()
    selContractsList$applyCode(params$genericCode)
    updateAll()
  })

  # "apply params to all" link
  observeEvent(input$applyParams, {
    params <- getParams()
    selContractsList$applyParams(params)
    updateAll()
  })
  
  # render list of contracts
  output$selContractsList <- renderUI({
    labels <- values$labels
    l <- length(labels)
    if (l>0) {
      list(
        tags$hr(),
        tags$h4("Selected contracts"),
        lapply(1:l, function(i){
          e <- labels[[i]]
          ckbxName <- paste0("selContractCkbx_",i)
          checkboxInput(ns(ckbxName), actionLink(ns(paste0(ckbxName,"_lk")), e$label), value=e$checked)
        }))
    }
  })
  
  # observe checkboxes
  lapply(1:20, function(i){
    ckbxName <- paste0("selContractCkbx_", i)
    observeEvent(input[[ckbxName]], {
      selContractsList$toggleChecked(i, input[[ckbxName]])
      updateAll()
    })
    observeEvent(input[[paste0(ckbxName,"_lk")]], {
      selContractsList$selectID <- i
      updateParams()
    })
  })
  
  # change control panel title
  observe({
    output$titleAction <- renderText(ifelse(values$selectID>0, 
                                             "Update contract", 
                                             "Add contract"))
  })
  
  # toggle buttons panel
  output$actionButtons <- renderUI({
    if (values$selectID>0) {
      tagList(   actionButton(ns("updateContract"), "Update"),
                 actionButton(ns("deleteContract"), "Delete"),
                 actionButton(ns("addContract"), "Add"),
                 conditionalPanel(paste0("input['", ns("contractType"), "'] != 'single'"),
                                  actionLink(ns("applyCode"), "Apply contract code to all"),br(),
                                  actionLink(ns("applyParams"), "Apply contract params to all"))
                 )
    } else {
      tagList(   actionButton(ns("addContract"), "Add contract"))
    }
  })
  
  reactive({values$series})
}