
# functions to compute rolled contracts series

Contract <- R6Class("Contract",

  public = list(
    
    initialize = function (params="", cache = TRUE) {
      private$data_src <- FuturesData$new()$singleton
      if (is.list(params)) {
        private$params_p <- params
        if (params$contractType == "single") {
          if (!is.null(params$completeCode)) {
            self$single(params$completeCode)
          }
        } else {
          if (!is.null(params$genericCode)) {
            private$genericCode <- params$genericCode
            rollDate <- ifNotNumeric(params$rollDate, 0)
            if (rollDate == 0) {
              self$price(params$contractNumber)
            } else if (rollDate == -1) {
              self$rollOnExpiry(params$contractNumber, params$leverage)
            } else if (rollDate == -2) {
              self$rollOnMaxOI(params$leverage)
            } else {
              self$monthlyRoll(rollDate, params$contractNumber, params$leverage, 
                               params$strategy, params$stratParams, stratCache = cache)
            }
          }
        }
      } else {
        private$genericCode <- params
        private$params_p$genericCode <- params
      }
      private$details_p <- private$data_src$tickers %>% 
        filter(`Quandl Code` == private$genericCode)
    },
    
    # return a single contract
    single = function (code) {
      
      private$params_p <- merge(list(
        completeCode = code,
        contractType = "single"
      ), private$params_p)

      private$contract_data <- private$data_src$data %>% 
        filter(completeCode == code) %>%
        arrange(Date) %>%
        mutate(Value = Settle,
               period = 1,
               Position = 1)
      
      private$params_p$genericCode <- private$contract_data$genericCode[1]
      
      private$contract_data %<>%
        select(Date, period, completeCode, expDate, Position, Value, logReturns)
      self
    },
    
    # paste prices - no roll
    price = function (contractNumber = 1, beforeExp = 0) {
      
      contractNumber %<>% ifNotNumeric(1)
      beforeExp %<>% ifNotNumeric(0)
      
      
      private$params_p <- merge(list(
        contractType = "roll",
        contractNumber = contractNumber
      ), private$params_p)
      
      cont <- private$data_src$data[
        genericCode == private$genericCode &
          Date <= maxDate-beforeExp]
      cont %<>%
        .[.[order(expDate), .I[min(.N, contractNumber)], keyby=Date]$V1] %>%
        .[order(Date),Value := Settle]
      
      codes <- unique(cont$completeCode)
      private$contract_data <- cont %>%
        mutate(period = match(completeCode, codes),
               Position = 1) %>%
        select(Date, period, completeCode, expDate, Position, Value, logReturns)
      self
    },
    
    # roll when max OI change
    rollOnMaxOI = function (leverage = 1) {
      leverage %<>% ifNotNumeric(1)

      private$params_p <- merge(list(
        contractType = "roll",
        leverage = leverage
      ), private$params_p)
      
      cont <- private$data_src$data %>% 
        filter(genericCode == private$genericCode) %>%
        group_by(Date) %>%
        arrange(desc(OI)) %>%
        filter(row_number(OI)==1) %>%
        ungroup() %>%
        arrange(Date)%>%
        mutate(logReturns = log((exp(logReturns)-1)*leverage+1)) %>%
        mutate(Value = exp(cumsum(logReturns)))
      codes <- unique(cont$completeCode)
      l<-length(cont$Settle)
      mult <- cont$Settle[l]/cont$Value[l]
      private$contract_data <- cont %>% 
        mutate(Value = Value*mult,
               period = match(completeCode, codes),
               Position = 1) %>%
        select(Date, period, completeCode, expDate, Position, Value, logReturns)
      self
    },
    
    # return contract rolled on expiry date
    rollOnExpiry = function (contractNumber = 1, leverage = 1) {
      contractNumber %<>% ifNotNumeric(1)
      leverage %<>% ifNotNumeric(1)
      
      private$params_p <- merge(list(
        contractType = "roll",
        contractNumber = contractNumber,
        leverage = leverage
      ), private$params_p)
      
      cont <- private$data_src$data[genericCode == private$genericCode]
      cont %<>%
        .[.[order(expDate), .I[min(.N, contractNumber)], keyby=Date]$V1] %>%
        ungroup() %>%
        .[order(Date), logReturns := log((exp(logReturns)-1)*leverage+1)] %>%
        .[, Value := exp(cumsum(logReturns))]
      
      l<-length(cont$Settle)
      mult <- cont$Settle[l]/cont$Value[l]
      codes <- unique(cont$completeCode)
      private$contract_data <- cont %>% 
        .[, `:=`(Value = Value*mult,
                 period = match(completeCode, codes),
                 Position = 1)] %>%
        .[,.(Date, period, completeCode, expDate, Position, Value, logReturns)]
      self
    },
    
    # monthly roll with strategy
    monthlyRoll = function (rollDate=1, contractNumber=1, leverage = 1, 
                            strategy = "long", stratParams = list(), stratCache = TRUE) {
      leverage %<>% ifNotNumeric(1)
      rollDate %<>% ifNotNumeric(1)
      contractNumber %<>% ifNotNumeric(1)
      
      strat <- Strategy$new(strategy, stratParams, stratCache)
      
      private$params_p <- merge(list(
        contractType = "roll",
        rollDate = rollDate,
        contractNumber = contractNumber,
        leverage = leverage,
        strategy = strat$name,
        stratParams = strat$args,
        stratCall = strat$call
      ), private$params_p)
      
      
      # add period id
      cont <- private$data_src$data[genericCode == private$genericCode]
      cont[, period := (year(Date)-1970)*12+month(Date)+(mday(Date)>=rollDate)*1]

      # find contract to select for each period
      sel <- cont %>%
        .[,quotationDays := max(Date)-min(Date), by=.(period, completeCode)] %>%
        .[.[,.I[quotationDays == max(quotationDays)], by=.(period, genericCode)]$V1] %>%
        .[,nbContracts:=.N, by=Date] %>%
        .[.[,.I[nbContracts == max(nbContracts) & Date == min(Date)], by=period]$V1] %>%
        .[.[order(expDate), .I[min(.N, contractNumber)], keyby=period]$V1] %>%
        .[,selContract:=completeCode]
      
      if (strat$name == "long") {
        private$contract_data <- sel[,.(period,selContract)] %>%
          .[cont, on="period"] %>%
          .[selContract==completeCode] %>%
          .[,logReturns := log((exp(logReturns)-1)*leverage+1)] %>%
          .[order(Date), Value := exp(cumsum(logReturns))] %>%
          .[order(Date),`:=`(Value = Value*last(Settle)/last(Value),
                             Position = 1)] %>%
          .[,.(Date, period, completeCode, expDate, Position, Value, logReturns)] %>%
          .[order(Date)]

        #         b1<- cont %>% 
        #   ungroup() %>%
        #   left_join(sel %>% select(period, selContract), by="period") %>%
        #   filter(completeCode == selContract) %>%
        #   arrange(Date) %>%
        #   mutate(logReturns = log((exp(logReturns)-1)*leverage+1)) %>%
        #   mutate(Value = exp(cumsum(logReturns))) %>% 
        #   mutate(Value = Value*last(Settle)/last(Value),
        #          Position = 1) %>% 
        #   select(Date, period, completeCode, expDate, Position, Value, logReturns)
        # 
      } else {
        
        sel %<>%
          ungroup() %>%
          arrange(Date) %>%
          left_join(strat$fun(.$genericCode[1], .$Date), by="Date")

        private$contract_data <- cont %>% 
          ungroup() %>%
          left_join(sel %>% select(period, selContract, Position), by="period") %>%
          filter(completeCode == selContract) %>%
          mutate(logReturns = log((exp(logReturns)-1)*leverage+1)) %>%
          group_by(period) %>% arrange(Date) %>%
          mutate(Value = (exp(cumsum(logReturns))-1)*Position+1) %>%
          group_by(period) %>% arrange(Date) %>%
          mutate(logReturns = diff(log(c(1, Value)))) %>%
          ungroup() %>%
          arrange(Date) %>%
          mutate(Value = exp(cumsum(logReturns))) %>%
          mutate(Value = Value*last(Settle)/last(Value)) %>% 
          select(Date, period, completeCode, expDate, Position, Value, logReturns)
      }
      self
    },
    
    checked = TRUE
    
  ),
  
  active = list(
    
    # return series data
    series = function(){
      private$contract_data
    },
    
    # series parameters
    params = function(){
      private$params_p
    },
    
    # series detailed info
    details = function(){
      private$details_p
    },
    
    # construct series label
    label = function() {
      if (self$params$contractType == "single") {
        expDate <- (self$series %>% select(expDate))$expDate[1]
        label <- paste(self$details$`IB Ticker`, "-", 
                       self$details$`Quandl Code`, "-", 
                       months(expDate, TRUE), year(expDate))
      } else {
        if (self$params$rollDate == 0) {
          rollLabel <- "price"
        } else if (self$params$rollDate == -1) {
          rollLabel <- "exp"
        } else if (self$params$rollDate == -2) {
          rollLabel <- "maxOI"
        } else {
          rollLabel <- paste("on", self$params$rollDate)
        }
        label <- paste0(self$details$`IB Ticker`, " - ", 
                       self$details$`Quandl Code`, " - ", 
                       "#", self$params$contractNumber, " - ",
                       rollLabel)
        if (!is.null(self$params$stratCall) && self$params$stratCall != "long") {
          label <- paste0(label, " - ", self$params$stratCall)
        }
        if (self$params$leverage!=1) {
          label <- paste0(label, " - ", "x", self$params$leverage)
        }
      }
      label
    }
    
  ),
  
  private = list(
    data_src = NULL,
    genericCode = "",
    details_p = list(),
    params_p = list(),
    contract_data = NULL
  )
)
