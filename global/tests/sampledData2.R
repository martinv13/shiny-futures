
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
      completeCodes <- .$completeCode
      .$data %>%
        setNames(completeCodes) %>%
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
    }) %>% ungroup() %>% select(-colNames) %>% data.table()

  # append expDate
  left_join(extr.data,
             raw.data %>% data.table() %>%
             select(genericCode, completeCode, expDate),
             by="completeCode") %>%
    group_by(completeCode) %>%
    arrange(Date) %>%
    filter(!is.na(Settle) & Settle !=0) %>%
    mutate(maxDate = max(Date),
           logReturns = c(0, diff(log(Settle)))) %>%
    ungroup()
}

if(!exists("gl_data") || is.null(gl_data)) {
  gl_data <- sampledData(gl_tickers)
}


