
# collection of contract data series
Contracts <- R6Class("Contracts",
  
  public = list(
    
    # add contract data to list
    add = function (contract = NULL) {
      if (!is.null(contract) ) {
        private$data <- c(private$data, contract)
        private$selId <- length(private$data)
        private$updated()
      }
    },
    
    # remove series from list
    delete = function () {
      if (private$selId > 0) {
        private$data <- private$data[-private$selId]
        private$selId <- min(length(private$data), max(1, private$selId - 1))
        private$updated()
      }
    },
    
    # update an item
    update = function (contract = NULL) {
      if (!is.null(contract) && private$selId>0) {
        private$data[[private$selId]] <- contract
        private$updated()
      }
    },
    
    # duplicate an item
    duplicate = function () {
      if (private$selId>0) {
        self$add(self$selected$clone())
      }
    },
    
    # apply contract code to all series
    applyCode = function(code) {
      if (private$selId > 0) {
        if (!is.null(code)) {
          private$data <- lapply(private$data, function(ctr){
            params <- ctr$params
            if (params$contractType != "single") {
              params$genericCode <- code
              Contract$new(params)
            } else {
              ctr
            }
          })
          private$updated()
        }
      }
    },
    
    # apply contract parameters to all series
    applyParams = function(params) {
      if (private$selId > 0) {
        if (!is.null(params) && params$contractType != "single") {
          private$data <- lapply(private$data, function(ctr){
            params$genericCode <- ctr$params$genericCode
            Contract$new(params)
          })
          private$updated()
        }
      }
    },
    
    # toggle visibility
    toggleChecked = function (id = -1, toogle = NULL) {
      if ( id>0 && id <= length(private$data)) {
        id2 <- id
      } else {
        id2 <- private$selId
      }
      if (id2 > 0) {
        if (is.logical(toogle)) {
          private$data[[id2]]$checked <- toogle
        } else {
          private$data[[id2]]$checked <- !is.logical(private$data[[id2]]$checked) || !private$data[[id2]]$checked
        }
      }
      private$updated()
    }
    
  ),
  
  active = list(
    
    # # return selected element
    # selected = function () {
    #   if (private$selId > 0 && private$selId <= length(private$data)) {
    #     private$data[[private$selId]]
    #   } else {
    #     NULL
    #   }
    # },
    # 
    # return or set selected ID
    selectID = function (i=-1) {
      if (i>=0 && i<=length(private$data)) {
        private$selId <- i
      }
      private$selId
    },
    
    # return params of selected element
    params = function () {
      if (self$selectID > 0) {
        private$data[[self$selectID]]$params
      } else{
        NULL
      }
    },
    
    # # return params of selected element (reactive)
    # paramsReactive = function () {
    #   reactive({
    #     if (private$rv$selId>0) {
    #       private$rv$data[[private$rv$selId]]$params  
    #     }
    #   })
    # },
    
    # return Multiseries object for further manipulation
    series = function () {
      private$series_p
    },
    
    labels = function () {
      uniques <- list()
      labels <- list()
      for (i in seq_along(private$data)) {
        e <- private$data[[i]]
        if (!is.null(uniques[[e$label]])) {
          suffix <- uniques[[e$label]]
          uniques[[e$label]] <- uniques[[e$label]] + 1
        } else {
          suffix <- NULL
          uniques[[e$label]] <- 1
        }
        labels[[i]] <- list(label=paste(c(e$label, suffix), collapse = " - "),
                            checked=e$checked)
      }
      labels
    }

  ),
  
  private = list(
    
    data = list(),
    selId = 0,
    series_p = NULL,
    
    updated = function(){
      private$series_p <- NULL
      if (length(private$data)>0) {
        labels <- self$labels
        checked <- labels %>% child.up("checked") %>% unlist()
        labels <- labels %>% child.up("label") %>% unlist()
        series <- private$data %>% child.up("series") %>% setNames(labels)
        if (sum(checked)>0) private$series_p <- MultiSeries$new(series[labels[checked]])
      }
    }

  )
)