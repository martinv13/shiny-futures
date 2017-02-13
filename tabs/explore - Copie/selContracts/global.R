

getContractData <- function(params) {

  params$rollDate <- as.numeric(params$rollDate)
  params$rollContract <- as.numeric(params$rollContract)

  if (!is.null(params)) {
    feat <- futuresData$tickers %>% filter(`Quandl Code` == params$genericCode)
    if (params$contractType == "single") {
      series <- Contracts$new()$single(params$completeCode)
      expDate <- (series %>% select(expDate))$expDate[1]
      label <- paste(feat$`IB Ticker`, "-", 
                     feat$`Quandl Code`, "-", 
                     months(expDate, TRUE), year(expDate))
    } else {
      series <- Contracts$new(params$genericCode)$monthlyRoll(rollDate = params$rollDate, 
                                                              contractNumber = params$rollContract, 
                                                              leverage = params$leverage)
      if (params$rollDate == 0) {
        rollLabel <- "price"
      } else if (params$rollDate == -1) {
        rollLabel <- "exp"
      } else if (params$rollDate == -2) {
        rollLabel <- "maxOI"
      } else if (params$rollDate == 32) {
        rollLabel <- "ms"
      } else {
        rollLabel <- paste0("on",params$rollDate)
      }
      label <- paste(feat$`IB Ticker`, "-", 
                     feat$`Quandl Code`, "-", 
                     "#", params$rollContract, "-",
                     rollLabel)
      if (params$leverage!=1) {
        label <- paste0(label, " - ", "x", params$leverage)
      }
    }
    rollpoints <- series %>%
      arrange(Date) %>%
      filter(completeCode != lag(completeCode)) %>%
        transmute(Date=Date,
                  Text=completeCode)
    return (list(series=series, label=label, rollpoints=rollpoints))
  }
}

