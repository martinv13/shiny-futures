
# portfolio backtesting

Portfolio <- R6Class("Portfolio",

###################

  public = list(
    
    # initialize with empty values or values list provided
    initialize = function (name, values = NULL) {
      private$fd <- FuturesData$new()$singleton
      private$db <- Db$new()$singleton
      private$name_p <- name
      print(paste("init", private$name_p))
      if (length(values) > 0) {
        if (!is.null(values$assetsClasses) && !is.null(values$assets) && 
            !is.null(values$strategies)) {
          private$classes_df <- data.table(values$assetsClasses) %>% select(Class, Weight)
          private$assets_df <- data.table(values$assets) %>% select(Code, `Contract #`, Weight)
          private$strats_df <- data.table(values$strategies) %>% select(Code, Strategy, Weight)
        }
      }
    },
    
    # save portfolio to database
    save = function () {
      escape <- function(x) {
        x <- gsub("([\\])", "\\\\", x)
        x <- gsub("([\"])", "\\\"", x)
        x
      }
      escape2 <- function(x) escape(escape(x))
      quer <- paste0("INSERT OR REPLACE INTO portfolios(name, assetClasses, assets, strategies) VALUES(",
                    "'",private$name_p,"', '",
                       escape2(toJSON(private$classes_df)), "', '",
                       escape2(toJSON(private$assets_df)), "', '",
                       escape2(toJSON(private$strats_df)), "')")
      res <- dbSendQuery(private$db$portableDBI, quer)
      dbClearResult(res)
    },

    daily = FALSE,
  
    toogleDaily = function (dailyi = NULL) {
      if (is.null(dailyi)) {
        self$daily <- !self$daily
        TRUE
      } else {
        chg <- self$daily == dailyi
        self$daily <- dailyi
        !chg
      }
    },

    # backtest portfolio (wrapper function with progress notification handling)
    backtest = function (outputFn = NULL) {
      print(paste("backtesting", private$name_p, "..."))
      totalTime <- 8 + self$adaptLeverage*10 + 4 + self$daily*5
      prPos <- NULL
      if (!is.null(outputFn)) {
        outputFn("Computing positions", 0)
        prPos <- function(valPos=0){
          outputFn(val=valPos*8/totalTime)
        }
      }
      self$computePositions_int(prPos)
      
      if (self$adaptLeverage) {
        print("adapt leverage...")
        if (!is.null(outputFn)) {
          outputFn("Adapting leverage", 8/totalTime)
        }
        self$adaptLeverage_int(self$quantileVaR, self$VaR)
      }
      
      if (self$daily) {
        print("daily backtest...")
        if (!is.null(outputFn)) {
          outputFn("Daily backtesting", ifelse(self$adaptLeverage, 18/totalTime, 8/totalTime))
        }
        self$dailyBacktest_int()
      } else {
        print("period backtest...")
        if (!is.null(outputFn)) {
          outputFn("Period backtesting", ifelse(self$adaptLeverage, 18/totalTime, 8/totalTime))
        }
#        self$periodBacktest_int()
        self$dailyBacktest_int_old()
      }
      
    },
    
    # compute positions
    computePositions_int = function (prPos=NULL) {

      rollOn <- self$rollDate
      rollOn %<>% ifNotNumeric(1)
      if (self$rounded) {
        self$notional %<>% ifNotNumeric(100)
      }
      
      futuresData <- FuturesData$new()$singleton
      
      # data.table of portfolio strategies with all attributes
      strats <- private$strats_df %>%
        left_join(futuresData$tickers %>%
                    mutate(Code = `IB Ticker`,
                           Class = Type,
                           genericCode = `Quandl Code`) %>%
                    select(Code, Class, genericCode, Multiplier, 
                           Currency, Margin, sizeEUR, nbContracts), 
                  by="Code") %>%
        mutate(stratWeight = Weight) %>%
        select(-Weight) %>%
        left_join(private$assets_df %>%
                    transmute(
                      Code = Code,
                      contractN = `Contract #`,
                      assetWeight = Weight),
                  by="Code") %>%
        left_join(private$classes_df %>%
                    transmute(Class = Class,
                              classWeight = Weight),
                  by = "Class") %>%
        filter(!is.na(genericCode) & 
                 !is.na(nbContracts) & nbContracts>0 &
                 !is.na(assetWeight) & assetWeight>0 & 
                 !is.na(stratWeight) & stratWeight>0 & 
                 !is.na(classWeight) & classWeight>0) %>%
        arrange(Code, Strategy) %>%
        as.data.table()
      
      # roll periods
      source <- futuresData$data %>% 
        filter(genericCode %in% (strats %>% distinct(genericCode))$genericCode) %>%
        mutate(period = (year(Date)-1970)*12 + month(Date) + (mday(Date)>=rollOn)*1)
      
      private$source_p <- source
      
      # roll dates for each genericCode
      rollPoints <- source %>%
        group_by(period, completeCode) %>%
        mutate(quotationDays = max(Date)-min(Date)) %>%
        group_by(completeCode) %>%
        mutate(maxPeriod = max(period),
               maxDate = max(Date))
      
      maxPer <- max(rollPoints$period)
      
      rollPoints %<>%
        group_by(period, genericCode) %>%
        filter(quotationDays == max(quotationDays) & (maxPeriod>period | period == maxPer)) %>% # & Date+7<maxDate
        group_by(period, Date) %>%
        mutate(nbContracts = n()) %>%
        group_by(period) %>%
        filter(nbContracts == max(nbContracts)) %>%
        filter(Date == min(Date)) %>%
        full_join(strats %>% distinct(genericCode, contractN), by="genericCode") %>%
        group_by(genericCode, period, contractN) %>%
        arrange(expDate) %>%
        filter(row_number(expDate) == min(n(), contractN)) %>%
        ungroup() %>% arrange(Date)
      
      # calculate positions for each asset and each strategy
      totalStrats <- dim(strats)[1]+6
      progress <- 3
      pos <- strats %>% 
        rowwise() %>% 
        do({
          ## progress bar
          if(!is.null(prPos)) prPos(progress/totalStrats)
          progress <<- progress+1
          ##
          
          row <- as.data.table(.)
          
          if (sum(unlist(lapply(row, is.null)),unlist(lapply(row, is.na)))>0) {
            bind_cols(row[0,],
                      data.table("Date"=as.Date(character()), "period"=integer(),
                                 "completeCode"=character(), "expDate"=as.Date(character()),
                                 "Position"=double(), "maxDate"=as.Date(character(0))))
          } else {
            strat <- Strategy$new(row$Strategy)

            # position at given points
            rollPoints %>%
              filter(genericCode == row$genericCode & contractN == row$contractN) %>%
              left_join(strat$fun(row$genericCode, .$Date), by="Date") %>%
              select(Date, period, completeCode, genericCode,
                     expDate, Position, maxDate) %>%
              left_join(row, by="genericCode", copy=TRUE)
          }
      }) %>% as.data.table()
      
      # aggregate positions and reweight
      
      # sum of strategies for each asset
      pos[,sumStrats := sum(stratWeight), by=.(Date,completeCode)]
      
      pos <- pos[assetWeight>0 & classWeight>0,]

      # number of assets for each asset class
      pos[, `:=`(nAssets=n_distinct(genericCode, contractN),
                 sumAssets=sum(assetWeight)), by=.(Date, Class)]
      pos[, Weight := assetWeight/nAssets*classWeight]
      
      pos <- pos[sumStrats > 0 & sumAssets > 0]
      
      # aggregate positions 
      pos <- pos[,.(Position = sum(Position*stratWeight/sumStrats)), 
                 by=.(Date, period, Code, genericCode, contractN, Class,
                      completeCode, sizeEUR, Margin, Weight, maxDate)]
      
      pos[,`:=`(
        rawPosition = Weight*Position,
        multiple = Weight*Position*self$notional*1000/sizeEUR,
        rounded = self$rounded*1
      )]
      
      pos[,rounded := ifelse(rounded==1, round(multiple-(self$minRound-0.5)*sign(multiple)), multiple)]
      
      # net position in % of nominal capital
      pos[,netPosition := rounded*sizeEUR/self$notional/1000]
      
      # apply leverage
      pos[,lev := ifelse(!is.null(self$leverage),self$leverage,1)]

      # pos %<>%
      #   group_by(Date, completeCode) %>%
      #   mutate(sumStrats = sum(stratWeight)) %>%
      #   group_by(Date, Class) %>%
      #   mutate(nAssets = n_distinct(genericCode),
      #          sumAssets =  sum(assetWeight)) %>%
      #   mutate(Weight = assetWeight/nAssets*classWeight) %>%
      #   filter(sumStrats > 0 & sumAssets > 0) %>%
      #   group_by(Date, period, Code, genericCode, contractN, Class,
      #            completeCode, sizeEUR, Margin, Weight, maxDate) %>%
      #   summarise(Position = sum(Position*stratWeight/sumStrats)) %>%
      #   mutate( rawPosition = Weight*Position,
      #           multiple = Weight*Position*self$notional*1000/sizeEUR,
      #           rounded = self$rounded*1) %>%
      #   mutate( rounded = ifelse(rounded==1, round(multiple-(self$minRound-0.5)*sign(multiple)), multiple)) %>%
      #   mutate(netPosition = rounded*sizeEUR/self$notional/1000) %>%
      #   mutate(lev = ifelse(!is.null(self$leverage),self$leverage,1)) %>%
      #   ungroup()
      
      private$positions_p <- pos
    },
    
    adaptLeverage_int = function (quantileVaR, targetVaR) {

      source <- private$source_p
      pos <- private$positions_p
      
      periodReturns <- pos %>% 
        group_by(genericCode, contractN) %>%
        arrange(Date) %>%
        mutate(nextDate = lead(Date))
      
      source %<>%
        group_by(completeCode) %>%
        arrange(Date) %>%
        mutate(previousSettle = lag(Settle))
      
      periodReturns %<>%
        left_join(source %>% transmute(Date=Date, completeCode=completeCode, startSettle = previousSettle),
                  by=c("Date", "completeCode")) %>%
        left_join(source %>% transmute(nextDate=Date, completeCode=completeCode, endSettle = previousSettle),
                  by=c("nextDate", "completeCode")) %>%
        mutate(periodReturn = endSettle/startSettle-1) %>%
        mutate(periodReturn = ifelse(is.na(periodReturn), 0, periodReturn))
      
      leverage <- periodReturns %>% 
        distinct(Date, period) %>% 
        mutate(VaR=NA) %>% arrange(Date)

      lookBack <- 20

      for (i in seq_along(leverage$Date)) {
        dReturns <- periodReturns %>% 
          filter(Date >= leverage$Date[i] %m+% months(-lookBack*12) &
                   Date < leverage$Date[i])
        if (dim(dReturns)[1]>30) {
          dReturns %<>%
            left_join(pos %>% 
                        filter(Date == leverage$Date[i]) %>%
                        select(genericCode, contractN, Position),
                      by=c("genericCode", "contractN")) %>%
            group_by(period) %>%
            summarise(periodReturn = sum(Weight*Position.y*periodReturn))
          tryCatch({
            leverage$VaR[i] <- as.numeric(
              exp(VaR(log(dReturns$periodReturn+1), p=quantileVaR))-1)
          }, warning = function(w){
            print(warning)
            leverage$VaR[i] <- NA
          })
        }
      }
      
      leverage %<>%
        mutate(leverage = ifelse(is.na(VaR), 1, -targetVaR/VaR))
      
      pos %<>% left_join(leverage, by=c("Date", "period")) %>%
        mutate(lev = leverage) %>%
        select(-leverage)
      
      private$positions_p <- pos
    },
    
    
    # backtest portfolio - daily values
    dailyBacktest_int_old = function () {
      
      source <- private$source_p
      pos <- private$positions_p
      
      # daily portfolio returns
      dailyReturns <- source %>% select(-period) %>%
        left_join(pos %>% select(genericCode, Date, period), by=c("Date", "genericCode")) %>%
        
        group_by(genericCode) %>%
        arrange(Date) %>%
        do(fill(.,period)) %>%
        select(-1) %>%
        filter(!is.na(period)) %>%
        
        left_join(pos %>% select(netPosition, lev, completeCode, period),
                  by=c("completeCode", "period")) %>%
        filter(!is.na(netPosition)) %>%
        
        group_by(completeCode, period) %>%
        arrange(Date) %>%
        mutate(pnl = exp(cumsum(logReturns))-1) %>%
        
        group_by(completeCode, period) %>%
        arrange(Date) %>%
        mutate(pnl = diff(c(0,pnl))) %>%
        
        group_by(Date) %>%
        summarise(period = first(period),
                  pnl = sum(pnl*netPosition*lev)) %>%
        
        group_by(period) %>%
        arrange(Date) %>%
        mutate(pnl = cumsum(pnl)) %>%
        
        group_by(period) %>%
        arrange(Date) %>%
        mutate(logReturns = diff(c(0, log(pnl+1)))) %>%
        
        ungroup() %>%
        arrange(Date) %>%
        mutate(Value = exp(cumsum(logReturns)))
      
      private$series_p <- dailyReturns %>% mutate(Returns=exp(logReturns)-1)
        
    },
    
    # backtest portfolio - daily values
    dailyBacktest_int = function () {
      
      source <- private$source_p
      pos <- private$positions_p
      
      # daily portfolio returns
      dr <- pos[,.(genericCode, Date, period)][
        source[,!"period",with=FALSE], 
        on=c("Date", "genericCode")]
      
      # propagate periods  
      dr <- dr[order(genericCode,Date),
               period := fill.na(period),
               by=genericCode][!is.na(period)]
      
      # join positions
      dr <- pos[,.(netPosition, lev, completeCode, period)][
        dr, on=c("completeCode", "period")][
        !is.na(netPosition),]
      
      # pnl of each position within a period
      dr[order(completeCode, period, Date),
         pnl := diff(c(0,exp(cumsum(logReturns))-1)),
         by=.(completeCode, period)]

      # pnl per day
      dr <- dr[order(Date),
               .("period" = first(period),
                 "pnl" = sum(pnl*netPosition*lev)),
               by=Date]
      
      # cumulate returns (arithmetic)
      dr[,pnl := cumsum(pnl),by=period]
      
      # compute log returns
      dr[,logReturns := diff(c(0, log(pnl+1))),by=period]
      
      # compute portfolio value
      dr[,`:=`("Value"=exp(cumsum(logReturns)),
              "Returns"=exp(logReturns)-1)]

      private$series_p <- dr
    },
    
    periodBacktest_int = function() {
      
      source <- private$source_p
      pos <- private$positions_p
      
      periodReturns <- pos %>% 
        group_by(genericCode, contractN) %>%
        arrange(Date) %>%
        mutate(nextDate = ifelse(is.na(lead(Date)), max(Date), lead(Date)))
      
      source %<>%
        group_by(completeCode) %>%
        arrange(Date) %>%
        mutate(previousSettle = lag(Settle))
      
      lastDate <- max(source$Date)
      
      periodReturns %<>%
        left_join(source %>% transmute(Date=Date, completeCode=completeCode, startSettle = previousSettle),
                  by=c("Date", "completeCode")) %>%
        left_join(source %>% transmute(nextDate=Date, completeCode=completeCode, endSettle = previousSettle),
                  by=c("nextDate", "completeCode")) %>%
        mutate(periodReturn = endSettle/startSettle-1) %>%
        mutate(periodReturn = ifelse(is.na(periodReturn), 0, periodReturn)) %>%
        group_by(period) %>%
        summarise(Date = min(Date),
                  periodReturns = sum(periodReturn*netPosition*lev)) %>%
        ungroup() %>%
        arrange(Date) %>%
        mutate(logReturns = log(periodReturns+1),
               Date=lead(Date, default=lastDate)) %>%
        mutate(Value = exp(cumsum(logReturns))) %>%
        filter(!is.na(Value))
      
      private$series_p <- periodReturns %>% mutate(Returns=periodReturns)
      TRUE
    },
    
      
    periodBacktest_int2 = function() {
          
      source <- private$source_p
      pos <- private$positions_p
      
      # period returns (simplified calculation)
      periodReturns <- source %>%
        group_by(completeCode, period) %>%
        filter(Date==min(Date)) %>%
        bind_rows(source %>%
                    group_by(genericCode) %>%
                    filter(period == max(period)) %>%
                    group_by(completeCode) %>%
                    filter(n()>7 & Date==max(Date)) %>%
                    mutate(period = period+1)) %>%
        group_by(completeCode) %>%
        arrange(period) %>%
        mutate(nextMonthSettle=lead(Settle)) %>%
        mutate(periodReturns=nextMonthSettle/Settle-1) %>%
        ungroup()
      
      lastDate <- max(periodReturns$Date)
      
      periodReturns %<>%
        left_join(pos %>% select(netPosition, lev, completeCode, period),
                  by=c("period", "completeCode")) %>%
        filter(!is.na(netPosition)) %>%
        group_by(period) %>%
        summarise(Date = min(Date),
                  periodReturns = sum(periodReturns*netPosition*lev)) %>%
        ungroup() %>%
        arrange(Date) %>%
        mutate(logReturns = log(periodReturns+1),
               Date=lead(Date, default=lastDate)) %>%
        mutate(Value = exp(cumsum(logReturns))) %>%
        filter(!is.na(Value))

      private$series_p <- periodReturns %>% mutate(Returns=periodReturns)
      TRUE
    },

    show = TRUE,
    selected = FALSE,
    rollDate = 1,
    rounded = FALSE,
    minRound = 0.6,
    notional = 100,
    adaptLeverage = FALSE,
    leverage = 1,
    quantileVaR = .95,
    VaR = 0.03


  ),
  
###################

  active = list(
    
    name = function(name = NULL) {
      if (!is.null(name)) {
        private$name_p <- name
      }
      private$name_p
    },
    
    # strategies
    strategies = function (df = NULL) {
      if (!is.null(df)) {
        private$strats_df <- df %>% select(Code, Strategy, Weight)
        private$strats_df$Strategy[is.na(private$strats_df$Strategy)] <- "long"
        private$strats_df$Weight[is.na(private$strats_df$Weight)] <- 1
        ret <- private$strats_df %>% left_join(private$fd$tickers %>%
                                                 mutate(Code=`IB Ticker`) %>%
                                                 select(Code, Type), by="Code")
        # add assets if necessary
        codes <- unique(private$strats_df$Code)
        newel <- codes[!(codes %in% private$assets_df$Code) & !is.na(codes) & codes!="NA"]
        l <- length(newel)
        if (l>0) {
          private$assets_df <- rbind(private$assets_df, 
                                  data.table("Code" = newel, 
                                             "Contract #" = rep(1,l),
                                             "Weight" = rep(1,l)))
        }
        # add asset class if necessary
        codes <- unique(ret$Type)
        newel <- codes[!(codes %in% private$classes_df$Class) & !is.na(codes) & codes!="NA"]
        l <- length(newel)
        if (l>0) {
          private$classes_df <- rbind(private$classes_df, 
                                      data.table("Class" = newel,
                                                 "Weight" = rep(1,l)))
        }
      } else {
        ret <- private$strats_df %>% left_join(data.table(private$fd$tickers) %>%
                                          mutate(Code=`IB Ticker`) %>%
                                          select(Code, Type), by="Code")
      }
      ret %>%
        left_join(self$assets %>% transmute(Code=Code, assetWeight=Total), by="Code") %>%
        group_by(Code) %>%
        mutate(sw = sum(Weight, na.rm=TRUE)) %>%
        mutate(Total = ifelse(sw>0,Weight/sw*assetWeight,0)) %>%
        ungroup() %>%
        select(Code, Type, Strategy, Weight, Total)
      
    },
    
    # assets
    assets = function (df = NULL) {
      if (!is.null(df)) {
        private$assets_df <- df %>% 
          group_by(Code) %>%
          summarise(`Contract #`=first(`Contract #`), Weight=sum(Weight, na.rm=TRUE)) %>%
          ungroup() %>%
          select(Code, `Contract #`, Weight)
        private$assets_df$Weight[is.na(private$assets_df$Weight)] <- 1
        private$assets_df$`Contract #`[is.na(private$assets_df$`Contract #`)] <- 1
        ret <- private$assets_df %>% left_join(private$fd$tickers %>%
                                                 mutate(Code=`IB Ticker`) %>%
                                                 select(Code, Type), by="Code")
        # add asset class if necessary 
        codes <- unique(ret$Type)
        newel <- codes[!(codes %in% private$classes_df$Class) & !is.na(codes) & codes!="NA"]
        l <- length(newel)
        if (l>0) {
          private$classes_df <- rbind(private$classes_df, 
                                  data.table("Class" = newel,
                                             "Weight" = rep(1,l)))
        }
      } else {
        ret <- private$assets_df %>% left_join(data.table(private$fd$tickers) %>%
                                                 mutate(Code=`IB Ticker`) %>%
                                                 select(Code, Type), by="Code")
      }
      ret %>% left_join(self$assetsClasses %>%
                          transmute(Type=Class,
                                    classWeight=Weight), by="Type") %>%
        group_by(Type) %>%
        mutate(aw=sum(Weight>0, na.rm=TRUE)) %>%
        mutate(Total=Weight*ifelse(aw>0,1/aw,0)*classWeight) %>%
        ungroup() %>%
        select(Code, Type, `Contract #`, Weight, Total)
    },
    
    # assets classes
    assetsClasses = function (df = NULL) {
      if (!is.null(df)) {
        private$classes_df <- df %>% 
          group_by(Class) %>%
          summarise(Weight=sum(Weight, na.rm=TRUE)) %>%
          ungroup() %>%
          select(Class, Weight)
      }
      private$classes_df
    },
    
    # portfolio backtested returns
    series = function () {
      if (!is.null(private$series_p)) {
        private$series_p %>% mutate(series = private$name_p)
      } else {
        NULL
      }
    },
    
    # portfolio positions
    positions = function () {
      private$positions_p
    }
    
    
  ),

######################
  
  private = list(
    name_p = "testPortfolio",
    
    fd = NULL,
    db = NULL,
    
    classes_df = data.table(
      "Class" = c("Bonds", "Equity"),
      "Weight" = c(1.5, .6)),
    
    assets_df = data.table(
      "Code" = c("ESTX50", "ES", "ZN", "GBL"),
      "Contract #" = rep(1,4),
      "Weight" = rep(1,4)),
    
    strats_df = data.table(
      "Code" = c("ESTX50", "ES", "ZN", "GBL"),
      "Strategy" = rep("long", 4),
      "Weight" = rep(1,4)),
    
    results = NULL,
    
    lastBacktest = NULL,
    lastBacktestFreq = NULL,
    
    series_p = NULL,
    source_p = NULL,
    positions_p = NULL

  )
)

