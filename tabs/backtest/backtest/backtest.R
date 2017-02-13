
backtest <- function(portfolio, rollOn=1, daily=FALSE) {
  
  daily <- portfolio$daily
  daily <- ifelse(is.null(daily), FALSE, daily)

  # add tickers details
  portfolio <- portfolio$strategies %>% mutate(stratWeights=Weights) %>%
    left_join(as.data.table(gl_tickers) %>% 
                transmute(Code=`IB Ticker`,
                          Type=Type,
                          genericCode=`Quandl Code`,
                          Name=Name,
                          Multiplier=Multiplier,
                          Currency=Currency), 
              by="Code") %>%
    left_join(portfolio$assets %>%
                transmute(Code=Code,
                          assetWeights=Weights,
                          contractN=`Contract #`), by="Code") %>%
    left_join(portfolio$assetClasses %>% 
                mutate(Type=Class,
                       classWeights=Weights), by="Type")

  # roll periods
  source <- gl_data %>% 
    filter(genericCode %in% (portfolio %>% distinct(genericCode))$genericCode) %>%
    mutate(period = (year(Date)-1970)*12 + month(Date) + (mday(Date)>=rollOn)*1) %>%
    left_join(portfolio %>% select(genericCode, contractN), by="genericCode")
  
  maxperiod <- max(source$period)
  
  # roll dates
  rollPoints <- source %>%
    group_by(completeCode) %>%
    mutate(maxPeriod = max(period), minPeriod = min(period)) %>%
    filter((minPeriod < period) & (maxPeriod > period | period==maxperiod)) %>%
    group_by(genericCode, Date) %>%
    mutate(nbContracts = n()) %>%
    group_by(genericCode, period) %>%
    filter(nbContracts == max(nbContracts)) %>%
    filter(Date == min(Date)) %>%
    group_by(genericCode, period) %>%
    arrange(expDate) %>%
    filter(row_number(expDate) == min(n(), contractN)) %>%
    ungroup() %>% arrange(Date)
  
  # calculate positions
  pos <- portfolio %>% rowwise() %>% do({
    if (sum(unlist(lapply(., is.null)),unlist(lapply(., is.na)))>0) {
      data.table("Date"=as.Date(character()), "period"=integer(), 
                 "genericCode"=character(), "completeCode"=character(), 
                 "expDate"=as.Date(character()), "Position"=double(), 
                 "stratWeight"=double(),"assetWeight"=double(),"classWeight"=double())
    } else {
      genericCode <- .$genericCode
      stratWeight <- .$stratWeights
      assetWeight <- .$assetWeights
      classWeight <- .$classWeights
      assetClass <- .$Type
      code <- .$genericCode

      # strategy to apply
      strat <- .$Strategy
      strat <- strsplit(strat,"(", fixed=TRUE)[[1]]
      stratFun <- strategies[[strat[1]]]
      if (is.null(stratFun)) {
        warning("strategy function not found")
        stratFun <- strategies[["long"]]
      }
      if (length(strat)>1) {
        stratArgs <- as.list(as.numeric(strsplit(strsplit(strat[2],")", fixed=TRUE)[[1]],",")[[1]]))
      } else {
        stratArgs <- list()
      }
      

      # position at given points
      rollPoints %>%
        filter(genericCode == code) %>%
        mutate(Position = do.call(stratFun, c(list(code, .$Date), stratArgs)),
               class=assetClass,
               stratWeight = stratWeight,
               assetWeight = assetWeight,
               classWeight = classWeight) %>%
        select(Date, period, genericCode, completeCode, class, Position, 
               stratWeight, assetWeight, classWeight)
    }
  }) %>% data.table() %>% ungroup()
  
  # aggregate positions and reweight
  pos %<>%
    group_by(period, genericCode) %>%
    mutate(assetSum=sum(stratWeight)) %>%
    group_by(period, class) %>%
    mutate(classSum=sum(assetWeight)) %>%
    ungroup() %>%
    mutate(wPosition = Position*stratWeight/assetSum*assetWeight/classSum*classWeight) %>%
    group_by(Date, period, genericCode, completeCode, class) %>%
    summarise(Position=sum(wPosition)) %>%
    ungroup()
  
  if (daily) {
    # daily portfolio returns
    dailyReturns <- source %>%
      ungroup() %>%
      left_join(pos %>% select(Position, completeCode, period),
                by=c("completeCode", "period")) %>%
      filter(!is.na(Position)) %>%
      group_by(completeCode, period) %>%
      arrange(Date) %>%
      mutate(pnl = exp(cumsum(logReturns))-1) %>%
      group_by(Date) %>%
      summarise(period = first(period),
                pnl = sum(pnl*Position)) %>%
      group_by(period) %>%
      arrange(Date) %>%
      mutate(logReturns = diff(c(0, log(pnl+1)))) %>%
      ungroup() %>%
      arrange(Date) %>%
      mutate(value = exp(cumsum(logReturns)))
    returns <- dailyReturns %>% mutate(Returns=exp(logReturns)-1)
    
  } else {
    
    # period returns
    periodReturns <- source %>%
      group_by(completeCode, period) %>%
      filter(Date==min(Date)) %>%
      group_by(completeCode) %>%
      arrange(period) %>%
      mutate(nextMonthSettle=lead(Settle)) %>%
      mutate(periodReturns=nextMonthSettle/Settle-1) %>%
      ungroup() %>%
      left_join(pos %>% select(Position, completeCode, period),
                by=c("completeCode", "period")) %>%
      filter(!is.na(Position)) %>%
      group_by(period) %>%
      summarise(Date = min(Date),
                periodReturns = sum(periodReturns*Position)) %>%
      ungroup() %>%
      arrange(Date) %>%
      mutate(logReturns = log(periodReturns+1),
             Date=lead(Date)) %>%
      mutate(value = exp(cumsum(logReturns)))
    periodReturns$Date[length(periodReturns$Date)] <- max(source$Date)
    returns <- periodReturns %>% mutate(Returns=periodReturns)
  }

  return(list(positions=pos,
              returns=returns %>% ungroup() %>% select(Date, Returns)))
#  return(list(dailyReturns, periodReturns))
}
  