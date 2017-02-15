
individualContract <- function (code) {
  gl_data %>% filter(completeCode == code) %>%
    arrange(Date) %>%
    mutate(Value = Settle) %>%
    select(Date, completeCode, Value, expDate)
}

continuousPrice <- function (code, contractNumber = 1) {
  contractNumber %<>% ifNotNumeric(1)
  cont <- gl_data %>% 
    filter(genericCode == code)
  cont %>%
    group_by(Date) %>%
    arrange(expDate) %>%
    filter(row_number(expDate) == min(contractNumber, n())) %>%
    mutate(Value = Settle) %>% 
    select(Date, completeCode, Value)
}

rollOnMaxOI <- function (code) {
  cont <- gl_data %>% 
    filter(genericCode == code) %>%
    group_by(Date) %>%
    arrange(desc(OI)) %>%
    filter(row_number(OI)==1) %>%
    ungroup() %>%
    arrange(Date)%>%
    mutate(Value = exp(cumsum(log((exp(logReturns)-1)*leverage+1))))
  l<-length(cont$Settle)
  mult <- cont$Settle[l]/cont$Value[l]
  cont %>% 
    mutate(Value = Value*mult) %>%
    select(Date, completeCode, Value)
}

rollOnExpiry <- function (code, contractNumber = 1, leverage = 1) {
  contractNumber %<>% ifNotNumeric(1)
  leverage %<>% ifNotNumeric(1)
  cont <- gl_data %>% 
    filter(genericCode == code) %>%
    group_by(Date) %>%
    arrange(expDate) %>%
    filter(row_number(expDate) == min(contractNumber, n())) %>%
    ungroup() %>%
    arrange(Date) %>%
    mutate(Value = exp(cumsum(log((exp(logReturns)-1)*leverage+1))))
  l<-length(cont$Settle)
  mult <- cont$Settle[l]/cont$Value[l]
  cont %>% 
    mutate(Value = Value*mult) %>%
    select(Date, completeCode, Value)
}

rollOnDate <- function (code, rollDate=1, contractNumber=1, 
                        leverage = 1, strategy = "long", stratParams = list()) {
  leverage %<>% ifNotNumeric(1)
  rollDate %<>% ifNotNumeric(1)
  contractNumber %<>% ifNotNumeric(1)

  if (strategy != "long" && is.null(strategies[[strategy]])) {
    strategy <- "long"
  }
  
  # add period id
  cont <- gl_data %>% 
    filter(genericCode == code) %>%
    mutate(period = (year(Date)-1970)*12+month(Date)+(mday(Date)>=rollDate)*1)
  maxperiod <- max(cont$period)
  
  # find contract to select for each period
  sel <- cont %>%
    group_by(completeCode) %>%
    mutate(maxPeriod = max(period), minPeriod = min(period)) %>%
    group_by(period) %>%
    filter((minPeriod < period) & (maxPeriod > period | period==maxperiod)) %>%
    group_by(Date) %>%
    mutate(nbContracts = n()) %>%
    group_by(period) %>%
    filter(nbContracts == max(nbContracts)) %>%
    filter(Date == min(Date)) %>%
    arrange(expDate) %>%
    filter(row_number(expDate) == min(n(), contractNumber)) %>%
    mutate(selContract = completeCode) %>% 
    ungroup()
  
  if (strategy == "long") {
    cont %>% 
      ungroup() %>%
      left_join(sel %>% select(period, selContract), by="period") %>%
      filter(completeCode == selContract) %>%
      arrange(Date) %>%
      mutate(Value = exp(cumsum(logReturns))) %>% 
      mutate(Value = Value*last(Settle)/last(Value)) %>% 
      select(Date, completeCode, Value)
  } else {
    sel %<>%
      arrange(Date) %>%
      mutate(Position = do.call(strategies[[strategy]], 
                                c(list(genericCode[1], .$Date), stratParams)))
    cont %>% 
      ungroup() %>%
      left_join(sel %>% select(period, selContract, Position), by="period") %>%
      filter(completeCode == selContract) %>%
      group_by(period) %>%
      arrange(Date) %>%
      mutate(Value = (exp(cumsum(logReturns))-1)*Position+1) %>%
      mutate(logReturns = diff(log(c(1, Value)))) %>%
      ungroup() %>%
      arrange(Date) %>%
      mutate(Value = exp(cumsum(logReturns))) %>%
      mutate(Value = Value*last(Settle)/last(Value)) %>% 
      select(Date, completeCode, Value)
  }
}


rolledContract <- function(code, rollOn=0, contractNumber=1, leverage=1, strategy="long", stratParams=NULL) {
  
  dailyRet <- gl_data %>% filter(genericCode == code)
  leverage <- as.numeric(leverage)
  if (is.na(leverage)) leverage <- 1
  
  rolled <- list()
  
  # do not roll (paste settlement prices)
  if (rollOn == 0) {
    cont <- dailyRet %>%
      group_by(Date) %>%
      arrange(expDate) %>%
      filter(row_number(expDate) == min(contractNumber, n())) %>%
      mutate(Value = Settle)
    
    # roll on expiry date
  } else if (rollOn == -1) {
    cont <- dailyRet %>%
      group_by(Date) %>%
      arrange(expDate) %>%
      filter(row_number(expDate) == min(contractNumber, n())) %>%
      ungroup() %>%
      arrange(Date) %>%
      mutate(Value = exp(cumsum(log((exp(logReturns)-1)*leverage+1))))
    l<-length(cont$Settle)
    mult <- cont$Settle[l]/cont$Value[l]
    cont %<>% mutate(Value = Value*mult)
    
    # roll on max OI
  } else if (rollOn == -2) {
    cont <- dailyRet %>%
      group_by(Date) %>%
      arrange(desc(OI)) %>%
      filter(row_number(OI)==1) %>%
      ungroup() %>%
      arrange(Date)%>%
      mutate(Value = exp(cumsum(return)))
    cont %<>% mutate(Value = Value*last(Settle)/last(Value))
    
    # roll on a given day of month
  } else {
    # add period id
    cont <- dailyRet %>% 
      mutate(period = (year(Date)-1970)*12+month(Date)+(mday(Date)>=rollOn)*1)
    maxperiod <- max(cont$period)
    # find contract to select for each period
    sel <- cont %>%
      group_by(completeCode) %>%
      mutate(maxPeriod = max(period), minPeriod = min(period)) %>%
      group_by(period) %>%
      filter((minPeriod < period) & (maxPeriod > period | period==maxperiod)) %>%
      group_by(Date) %>%
      mutate(nbContracts = n()) %>%
      group_by(period) %>%
      filter(nbContracts == max(nbContracts)) %>%
      filter(Date == min(Date)) %>%
      arrange(expDate) %>%
      filter(row_number(expDate) == min(n(), contractNumber)) %>%
      mutate(selContract = completeCode) %>% ungroup()
    
    cont %<>% ungroup() %>%
      left_join(sel %>% select(period, selContract), by="period") %>%
      filter(completeCode == selContract) %>%
      arrange(Date) %>%
      mutate(Value = exp(cumsum(logReturns)))
    
    cont %<>% mutate(Value = Value*last(Settle)/last(Value))
  }
  
  cont %>% select(Date, completeCode, Value)
}


