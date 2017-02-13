
# class used to define and call strategies

Strategy <- R6Class("Strategy",
                      
  public = list(
    
    initialize = function(strat=NULL, stratParams=NULL, cache=TRUE) {
      
      if (!is.null(strat)){
        
        strat <- strsplit(strat,"(", fixed=TRUE)[[1]]
        stratName <- strat[1]
        stratFun <- private$strats[[strat[1]]]
        
        if (is.null(stratFun)) {
          warning("strategy function not found")
          stratFun <- private$strats[["long"]]
          stratName <- "long"
          stratParams <- list()
        }
        
        schem <- formals(stratFun)[-(1:2)]
        
        if (length(schem)>0) {
          
          if (length(strat)>1) {
            stratParams <- unlist(strsplit(strsplit(strat[2],")", fixed=TRUE)[[1]],",")[[1]])
            suppressWarnings({
              stratParams <- as.list(ifelse(is.na(as.numeric(stratParams)), stratParams, as.numeric(stratParams)))
            })
          }
          if (is.null(stratParams) || length(stratParams)==0) {
            stratParams <- schem
          } else {
            if (is.null(names(stratParams))) {
              l <- min(length(stratParams), length(schem))
              names(stratParams)[1:l] <- names(schem)[1:l]
            }
            stratParams <- stratParams[names(stratParams) %in% names(schem)]
            stratParams <- merge(stratParams, schem)
          }
          stratArgs <- stratParams[names(schem)]
        } else {
          stratArgs <- list()          
        }

        self$name <- stratName
        self$args <- stratArgs
        self$cache <- cache
        self$call <- paste0(stratName,ifelse(length(stratArgs)>0,
                                             paste0("(",paste0(unlist(stratArgs), collapse = ","),")"),""))
        dbDPL <- Db$new()$singleton$localDPL
        dbDBI <- Db$new()$singleton$localDBI
        
        self$fun <- function (code, dates) {
          if ((self$name %in% c("long", "short")) || !self$cache) {
            do.call(stratFun, c(list(code, dates), stratArgs))
          } else {
            # getting results from cache
            signat <- digest(as.character(body(stratFun)))
            keys <- data.table(
              "Date"=format(dates, "%Y%m%d"),
              "genericCode"=code,
              "call"=self$call,
              "signature"=signat)
            # retrieve existing results from db
            res <- dbSendQuery(dbDBI, "DROP TABLE IF EXISTS temp_cache_quer")
            dbClearResult(res)
            cached <- copy_to(dbDPL, keys, "temp_cache_quer", temporary=FALSE) %>%
              left_join(tbl(dbDPL, "strats_cache"),
                                         by=c("Date", "genericCode", "call", "signature")) %>%
              collect() %>%
              mutate(Date = as.Date(Date, format="%Y%m%d"))
            dbDBI %>% db_drop_table("temp_cache_quer")
            
            if (sum(is.na(cached$Position)>0)) {
              if (self$name %in% c("carryMA")) {
                missings <- cached
                res <- dbSendQuery(db$localDBI, 
                                   paste0("delete from strats_cache where genericCode=\"",
                                          code,  "\" and call=\"", self$call,"\""))
                dbClearResult(res)
              } else {
                missings <- cached %>% filter(is.na(Position))
              }
              print(paste("computing", self$call, "for", code, "(", dim(missings)[1], ")"))
              bcktest <- do.call(stratFun, c(list(code,missings$Date), stratArgs))
              l <- dim(bcktest)[2]
              if (l>2) {
                bcktest <- bcktest[,1:min(l,6)]
                colnames(bcktest)[3:min(l,6)] <- paste0("v", 1:min(l-2, 4))
              }
              missings <- bcktest %>% 
                mutate(genericCode=code,
                       call=self$call,
                       signature=signat)
              dbDBI %>% db_insert_into("strats_cache", 
                                       missings %>% mutate(Date=format(Date, "%Y%m%d")))
              cached %<>% 
                filter(!is.na(Position)) %>% 
                bind_rows(missings)
            }
            cached %>%
              select(-genericCode, -call) %>%
              arrange(Date) %>%
              data.table()
          }
        }
      }
    },
    
    name = NULL,
    fun = NULL,
    args = list(),
    call = NULL,
    cache = TRUE

  ),
  
  active = list(
    
    # list of strategies
    list = function () {
      names(private$strats)
    }
  ),
  
  private = list(
    
    # list of available strategies
    strats = list(
      
      # long position
      "long" = function (genericCode, dates){
        data.table(Date = dates,
                   Position=rep(1, length(dates)))
      },
      
      # short position
      "short" = function (genericCode, dates){
        data.table(Date = dates,
                   Position = rep(-1, length(dates)))
      },
      
      # # trend following position comparing absolute price level
      # "trendc" = function (genericCode, dates, window=30, contract=1){
      #   
      #   window %<>% ifNotNumeric(30)
      #   contract %<>% ifNotNumeric(1)
      #   
      #   series <- Contract$new(genericCode)$monthlyRoll(contractNumber = contract)$series
      # 
      #   # first dates strictly anterior to "dates"
      #   val1 <- unlist(lapply(dates, function(d){
      #     i <- match(TRUE, series$Date>=d)
      #     if (i>1) {
      #       series$Value[i-1]
      #     } else {
      #       0
      #     }
      #   }))
      #   val2 <- unlist(lapply(dates, function(d){
      #     i <- match(TRUE, series$Date>=d-window)
      #     if (i>1) {
      #       series$Value[i-1]
      #     } else {
      #       0
      #     }
      #   }))
      #   data.table(Date = dates,
      #              Position = (val1>val2)*2-1)
      # },
      # 
      # # trend following position : tunnel relative to residuals history
      # "trend" = function (genericCode, dates, window=90, quant=.5, pval=1, contract=1){
      #   window %<>% ifNotNumeric(90)
      #   contract %<>% ifNotNumeric(1)
      #   pval %<>% ifNotNumeric(1)
      #   quant %<>% ifNotNumeric(.5)
      #   
      #   series <- Contract$new(genericCode)$monthlyRoll(contractNumber = contract)$series
      #   
      #   series %<>% arrange(Date)
      #   sdates <- as.numeric(series$Date)
      #   sval <- log(series$Value)
      #   
      #   # residuals
      #   res <- rep(0, length(dates))
      #   # slope
      #   slp <- rep(0, length(dates))
      #   # take position base on breakout or slope
      #   test <- rep(FALSE, length(dates))
      # 
      #   for (i in seq_along(dates)) {
      #     delay <- pmax(0, dates[i] - series$Date)
      #     last <- match(TRUE, series$Date >= dates[i])
      #     last <- ifelse(is.na(last), length(series$Date), last-1)
      #     if (last > window) {
      #       w <- exp(-delay/window)
      #       if (length(w) > last) {
      #         w[(last+1):length(w)] <- 0
      #       }
      #       w[w<0.001] <- 0
      #       m <- lm(sval ~ sdates, weights = w)
      #       s <- summary(m)
      #       if (sum(dim(s$coefficients) == 1:2*2)==2) {
      #         res[i] <- sval[last] - (s$coefficients[1,1]+s$coefficients[2,1]*sdates[last])
      #         slp[i] <- ifelse(s$coefficients[2,4] < pval, s$coefficients[2,1], 0)
      #         if (i>8) {
      #           test[i] <- abs(res[i]) > quantile(abs(res[1:(i-1)]), quant, na.rm = TRUE)
      #         }
      #       }
      #     }
      #   }
      #   
      #   # residual sign
      #   ress <- (res>0)*2-1
      #   ress[is.na(ress)] <- 0
      #   # slope sign
      #   slps <- (slp>0)-(slp<0)
      #   slps[is.na(slps)] <- 0
      #   test[is.na(test)] <- FALSE
      # 
      #   data.table(
      #     Date = dates,
      #     Position = ifelse(test, ress, slps),
      #     Residual = res,
      #     Slope = slp,
      #     Criteria = test*1)
      # },
      
      # trend following position : slopes over different time ranges
      "trendS" = function (genericCode, dates, window="30-90-200", slope=1, slopeAcc=0, breakout=0, bkoutSign=0, contract=1){

        contract %<>% ifNotNumeric(1)
        slope %<>% ifNotNumeric(1)
        breakout %<>% ifNotNumeric(0)
        slopeAcc %<>% ifNotNumeric(0)
        bkoutSign %<>% ifNotNumeric(0)
        
        pval <- 0.01
        
        windows <- as.numeric(unlist(strsplit(window, "-") ))
        if (sum(is.na(windows))>0) windows <- c(30, 90, 200)
        
        series <- Contract$new(genericCode)$monthlyRoll(contractNumber = contract)$series
        
        series %<>% arrange(Date)
        sdates <- as.numeric(series$Date)
        sval <- log(series$Value)
        
        # slope
        slopes <- matrix(0, ncol = length(windows), nrow = length(dates))
        breakouts <- matrix(0, ncol = length(windows), nrow = length(dates))

        for (i in seq_along(dates)) {
          delay <- pmax(0, dates[i] - series$Date)
          last <- match(TRUE, series$Date >= dates[i])
          last <- ifelse(is.na(last), length(series$Date), last-1)
          if (last > max(windows)) {
            for (j in seq_along(windows)) {
              window <- windows[j]
              w <- exp(-delay/window)
              if (length(w) > last) {
                w[(last+1):length(w)] <- 0
              }
              w[w<0.001] <- 0
              m <- lm(sval ~ sdates, weights = w)
              s <- summary(m)
              if (sum(dim(s$coefficients) == 1:2*2)==2) {
                slopes[i, j] <- ifelse(s$coefficients[2,4] < pval, s$coefficients[2,1], 0)
                breakouts[i, j] <- sval[last] - s$coefficients[2,1]*sdates[last]-s$coefficients[1,1]
              }
            }
          }
        }
        
        slopes[is.na(slopes)] <- 0
        breakouts[is.na(breakouts)] <- 0
        
        Position <- 
          slope * rowSums((slopes>0)-(slopes<0))/dim(slopes)[2] +
          breakout * rowSums((breakouts>0)-(breakouts<0))/dim(breakouts)[2] +
          slopeAcc * (
            (rowSums(t(apply(slopes, 1, diff))<=0)==(dim(slopes)[2]-1)) -
            (rowSums(t(apply(slopes, 1, diff))>=0)==(dim(slopes)[2]-1))) +
          bkoutSign * (
            (rowSums((breakouts>=0))==dim(breakouts)[2]) -
            (rowSums((breakouts<=0))==dim(breakouts)[2]))
        
        Position <- Position / (abs(slope)+abs(breakout)+abs(slopeAcc)+abs(bkoutSign))

        data.table(
          "Date" = dates,
          "Position" = Position)
      },

      
      # "carry" position
      "carry" = function (genericCode, dates, gain=10){
        
        gain %<>% ifNotNumeric(10)
        
        series1 <- Contract$new(genericCode)$price(contractNumber=1, beforeExp=5)$series
        series2 <- Contract$new(genericCode)$price(contractNumber=2, beforeExp=5)$series
        
        carr<-series1 %>%
          transmute(Date=Date, front=Value, frontExp=expDate) %>%
          left_join(series2 %>% 
                      transmute(Date=Date, def=Value, defExp=expDate),
                    by="Date") %>%
          mutate(carry = log(front)-log(def)) %>%
          mutate(carry = exp(carry*365/as.numeric(defExp-frontExp))-1) %>%
          filter(carry<1) %>%
          full_join(data.table(Date=dates-1), by="Date") %>%
          arrange(Date) %>%
          fill(carry) %>%
          mutate(carry = stats::filter(carry, c(rep(1,10)/10,0), sides=1)) %>%
          mutate(Date = Date+1) %>%
          filter(Date %in% dates) %>%
          mutate(Position = ifelse(is.na(carry),0,carry)) %>%
          mutate(Position = pmax(-1,pmin(1,Position*gain))) %>%
          select(Date, Position) %>% data.table()
        
        carr
      },
      
      "carryMA" = function (genericCode, dates, MA=12, gain=10){
        
        gain %<>% ifNotNumeric(10)
        MA %<>% ifNotNumeric(12)
        
        series1 <- Contract$new(genericCode)$price(contractNumber = 1)$series
        series2 <- Contract$new(genericCode)$price(contractNumber = 2)$series
        
        series1 %>%
          filter(Date %in% dates) %>%
          transmute(Date=Date, front=Value, frontExp=expDate) %>%
          left_join(series2 %>% 
                      filter(Date %in% dates) %>%
                      transmute(Date=Date, def=Value, defExp=expDate),
                    by="Date") %>%
          mutate(carry = log(front)-log(def)) %>%
          mutate(carry = exp(carry*365/as.numeric(defExp-frontExp))-1) %>%
          mutate(carry = ifelse(is.na(carry),0,carry)) %>%
          arrange(Date) %>%
          mutate(Position = stats::filter(carry, rep(1,MA)/MA, sides=1)) %>%
          mutate(Position = ifelse(is.na(Position),0,Position)) %>%
          mutate(Position = pmax(-1,pmin(1,Position*gain))) %>%
          select(Date, Position) %>% data.table()
      },
      
      
      # 12-month "carry" position
      "carry12" = function (genericCode, dates, gain=10){
        
        gain %<>% ifNotNumeric(10)
        
        futuresData <- FuturesData$new()$singleton
        code <- genericCode
        
        carr <- futuresData$data %>%
          filter(genericCode == code) %>%
          filter(Date <= maxDate-5) %>%
          mutate(front = Settle,
                 frontExp = expDate,
                 defExp = expDate %m+% months(12))
        
        carr %<>%
          left_join(carr %>% transmute(
            Date = Date,
            defExp = expDate,
            def = Settle),
            by =c("Date", "defExp")) %>%
          filter(!is.na(def)) %>%
          group_by(Date) %>%
          arrange(frontExp) %>%
          filter(row_number(frontExp)==1) %>%
          mutate(carry = front/def-1)
        
        carr %<>% full_join(data.table(Date=dates-1), by="Date") %>%
          arrange(Date) %>%
          fill(carry) %>%
          # mutate(period = month(Date)+(year(Date)-1970)*12) %>%
          # group_by(period) %>%
          # do({
          #   . %>% 
          #     arrange(Date) %>%
          #     fill(carry)
          # }) %>%
          mutate(Date = Date+1) %>%
          filter(Date %in% dates) %>%
          mutate(Position = ifelse(is.na(carry),0,carry)) %>%
          mutate(Position = pmax(-1,pmin(1,Position*gain))) %>%
          select(Date, Position) %>% data.table()

        carr
      },
      
      # "carry" based on rate
      "carryR" = function (genericCode, dates, country="US", maturity=5, gain=1){
        
        gain %<>% ifNotNumeric(1)
        maturity %<>% ifNotNumeric(5)
        
        ratesData <- RatesData$new()$singleton
        
        if (country == "US") {
          data <- ratesData$data %>% 
            filter(shortName %in% c("US3M", "US2Y", "US5Y", "US10Y")) %>%
            spread(shortName, Value) %>%
            full_join(data.table(Date=dates-1), by="Date") %>%
            arrange(Date) %>%
            fill(US3M, US2Y, US5Y, US10Y) %>%
            mutate(Date = Date+1) %>%
            filter(Date %in% dates) %>%
            filter(!is.na(US3M))
          
          data %<>% 
            mutate(finRate = US3M) %>%
            bind_cols(
            apply( data %>% select(US3M,US2Y,US5Y,US10Y), 1, function(r){
              fun <- splinefun(c(0.25,2,5,10), r, method="monoH.FC")
              c(fun(maturity-1/12), fun(maturity))
            }) %>% t %>% data.table() %>% setNames(c("nextPeriodRate", "presentRate")))
          
        } else if (country == "DE") {
          data <- ratesData$data %>% 
            filter(shortName %in% c("DE6M", "DE2Y", "DE5Y", "DE10Y")) %>%
            spread(shortName, Value) %>%
            full_join(data.table(Date=dates-1), by="Date") %>%
            arrange(Date) %>%
            fill(DE6M, DE2Y, DE5Y, DE10Y) %>%
            mutate(Date = Date+1) %>%
            filter(Date %in% dates) %>%
            filter(!is.na(DE6M))
          
          data %<>% 
            mutate(finRate = DE6M) %>%
            bind_cols(
            apply( data %>% select(DE6M, DE2Y, DE5Y, DE10Y), 1, function(r){
              fun <- splinefun(c(0.5,2,5,10), r, method="monoH.FC")
              c(fun(maturity-1/12), fun(maturity))
            }) %>% t %>% data.table() %>% setNames(c("nextPeriodRate", "presentRate")))
        
        }
          
        data %>% 
          mutate(
            capitalGain = exp(-presentRate/100*(maturity-1/12))-
              presentRate/nextPeriodRate*(exp(-presentRate/100*(maturity-1/12))-1)-1,
            coupon = (presentRate/100+1)^(1/12)-1,
            financing = -((finRate/100+1)^(1/12)-1)) %>%
          mutate(carry = (capitalGain+coupon+financing+1)^12-1) %>%
          full_join(data.table(Date=dates), by="Date") %>%
          mutate(Position = ifelse(is.na(carry),0,carry)) %>%
          mutate(Position = pmax(-1,pmin(1,Position*gain))) %>%
          select(Date, Position) %>% data.table()
        
        # carr <- ratesData$data %>%
        #   filter(shortTerm == shortName) %>%
        #   mutate(st = Value/100) %>%
        #   full_join(ratesData$data %>%
        #               filter(longTerm == shortName) %>%
        #               transmute(Date=Date,
        #                         lt = Value/100), by="Date") %>%
        #   mutate(carry = lt-st) %>%
        #   full_join(data.table(Date=dates-1), by="Date") %>%
        #   arrange(Date) %>%
        #   fill(carry) %>%
        #   mutate(Date = Date+1) %>%
        #   filter(Date %in% dates) %>%
        #   mutate(Position = ifelse(is.na(carry),0,carry)) %>%
        #   mutate(Position = pmax(-1,pmin(1,Position*gain))) %>%
        #   select(Date, Position) %>% data.table()
        # carr
      }
      
      
      # # "value" position
      # "value" = function (genericCode, dates, window=6, contract=1){
      #   
      #   window %<>% ifNotNumeric(6)
      #   contract %<>% ifNotNumeric(1)
      #   
      #   series <- Contract$new(genericCode)$monthlyRoll(contractNumber = contract)$series
      #   
      #   pos <- rep(0, length(dates))
      #   
      #   wd <- round(window*365.25)
      #   
      #   s <- match(TRUE, dates > series$Date[1] + wd)
      #   l <- length(dates)
      #   
      #   if (l>=s) {
      #     for (i in s:l){
      #       sel <- (dates[i]-wd)<series$Date & series$Date<dates[i]
      #       if (sum(sel)>0) {
      #         mxw <- max(series$Value[sel])
      #         mnw <- min(series$Value[sel])
      #         pos[i] <- ifelse(mxw>mnw,(mxw-series$Value[sel][sum(sel)])/(mxw-mnw),0)*2-1
      #       }
      #     }
      #   }
      #   data.table(
      #     Date = dates,
      #     Position = pos)
      # },
      # 
      # # skew
      # "skew" =  function (genericCode, dates, window=30, gain=.1, contract=1){
      # 
      #   contract %<>% ifNotNumeric(1)
      #   window %<>% ifNotNumeric(30)
      #   gain %<>% ifNotNumeric(.1)
      #   
      #   series <- Contract$new(genericCode)$monthlyRoll(contractNumber = contract)$series
      #   
      #   series %<>% arrange(Date)
      #   sdates <- as.numeric(series$Date)
      # 
      #   skewVal <- rep(0, length(dates))
      # 
      #   for (i in seq_along(dates)) {
      #     sel <- series$Date<dates[i] & series$Date>=dates[i]-window
      #     if (sum(sel)>window/4) {
      #       skewVal[i] <- skewness(series$logReturns[sel])            
      #     }
      #   }
      #   
      #   data.table(
      #     Date = dates,
      #     Position = -skewVal
      #   ) %>%
      #     mutate(Position = ifelse(is.na(Position),0,Position)) %>%
      #     mutate(Position = pmax(-1,pmin(1,Position*gain)))
      # }
        
    )
    
  )
)

