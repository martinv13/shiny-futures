
sampledData <- function(tickers) {
  
  selectedContracts <- tickers$`Quandl Code`
  
  # extract data from db
  raw.data <- tbl(dbDPL, "contracts")
  if (length(selectedContracts) == 1) {
    raw.data %<>% filter(genericCode == selectedContracts)
  } else {
    raw.data %<>% filter(genericCode %in% selectedContracts)
  }
  raw.data %<>%
    collect() %>%
    mutate(expDate=as.Date(expDate, format="%Y%m%d"),
           genericCode2=genericCode) %>%
    separate(genericCode2, c("exchange", "code"), sep="/")
  
  if (length(raw.data$expDate)==0) {
    return (FALSE)
  }
  
  extr.data <- raw.data
  extr.data$data <- lapply(extr.data$data, function(x) {
    unserialize(x)
  })
  extr.data$colNames <- unlist(lapply(extr.data$data, function(x){
    paste(colnames(x), collapse="_")
  }))
  extr.data %<>%
    group_by(colNames) %>%
    do({
      .$data %>%
        setNames(.$completeCode) %>%
        bind_rows(.id="completeCode") %>%
        notNull(to="Date", "Date") %>%
        notNull(to="Settle", 
                "Settle", 
                "Settlement.Price",
                "Settlement Price",
                "Sett.Price",
                "Previous Settlement",
                "Previous.Settlement") %>%
        notNull(to="OI",
                "Prev. Day Open Interest",
                "Open Interest",
                "Open Interest (OI)",
                "Open.Interest",
                "Prev..Day.Open.Interest") %>%
        select(completeCode, Date, Settle, OI)
    }) %>% ungroup() %>% select(-colNames)

  
  
  # convert all data into a single data.table
  # CME data
  raw.data.cme <- raw.data %>% filter(exchange == "CME")
  data.cme <-lapply(raw.data.cme$data, function(x) {unserialize(x)} ) %>%
    setNames(raw.data.cme$completeCode) %>%
    bind_rows(.id="completeCode") %>%
    data.table() %>%
    transmute(completeCode=completeCode,
              Date=Date,
              Settle=Settle,
              Volume=Volume,
              OI=pmax(`Prev. Day Open Interest`, `Open Interest`, na.rm=TRUE))
  
  # EUREX data
  raw.data.eurex <- raw.data %>% filter(exchange == "EUREX")
  data.eurex <-lapply(raw.data.eurex$data, function(x) {unserialize(x)} ) %>%
  setNames(raw.data.eurex$completeCode) %>%
  bind_rows(.id="completeCode") %>%
  data.table() %>%
  transmute(completeCode=completeCode,
            Date=Date,
            Settle=Settle,
            Volume=Volume,
            OI=`Prev. Day Open Interest`)

  # asx data
  raw.data.asx <- raw.data %>% filter(exchange == "ASX")
  data.asx <-lapply(raw.data.asx$data, function(x) {unserialize(x)} ) %>%
  setNames(raw.data.asx$completeCode) %>%
  bind_rows(.id="completeCode") %>%
  data.table() %>%
  transmute(completeCode=completeCode,
            Date=Date,
            Settle=pmax(Settle, Previous.Settlement, na.rm=TRUE),
            Volume=Volume,
            OI=Open.Interest..OI.)

  # SGX data
  raw.data.sgx <- raw.data %>% filter(exchange == "SGX")
  data.sgx <-lapply(raw.data.sgx$data, function(x) {unserialize(x)} ) %>%
  setNames(raw.data.sgx$completeCode) %>%
  bind_rows(.id="completeCode") %>%
  data.table() %>%
  transmute(completeCode=completeCode,
            Date=Date,
            Settle=Settle,
            Volume=Volume,
            OI=Prev..Day.Open.Interest)
  
  # OSE data
  raw.data.ose <- raw.data %>% filter(exchange == "OSE")
  data.ose <-lapply(raw.data.ose$data, function(x) {
      temp<-unserialize(x)
      # colnames(temp)<-c("Date","Settlement.Price", "Theoretical.Price", 
      #                   "Underlying.Index", "Interest.Rate", "Days.until.Maturity")
      temp
    }) %>%
    setNames(raw.data.ose$completeCode) %>%
    bind_rows(.id="completeCode") %>%
    data.table() %>%
    transmute(completeCode=completeCode,
              Date=Date,
              Settle=`Settlement.Price`,
              Volume=NA,
              OI=NA)
  
  # MX data
  raw.data.mx <- raw.data %>% filter(exchange == "MX")
  data.mx <-lapply(raw.data.mx$data, function(x) {unserialize(x)} ) %>%
  setNames(raw.data.mx$completeCode) %>%
  bind_rows(.id="completeCode") %>%
  data.table() %>%
  transmute(completeCode=completeCode,
            Date=Date,
            Settle=Settlement.Price,
            Volume=Volume,
            OI=Prev..Day.Open.Interest)
  
  # append data
  data <- bind_rows(data.cme, data.eurex , data.sgx, data.asx, data.mx)
 
  # append expDate
  left_join(data,
             raw.data %>%
             select(genericCode, completeCode, expDate),
             by="completeCode") %>%
    group_by(completeCode) %>%
    arrange(Date) %>%
    filter(!is.na(Settle) & Settle !=0) %>%
    mutate(maxDate = max(Date),
           logReturns = c(0, diff(log(Settle)))) %>%
    ungroup()
}

gl_data <- sampledData(gl_tickers)


