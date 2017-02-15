
strat_cache <- list()

strategies <- list(
  
  # long position
  "long" = function (genericCode, dates){
    rep(1, length(dates))
  },
  
  
  # short position
  "short" = function (genericCode, dates){
    rep(-1, length(dates))
  },
  
  
  # trend following position
  "trend" = function (genericCode, dates, window=30, contract=1){

      key <- paste("roll", genericCode, contract, sep="_")
      
      series <- strat_cache[[key]]
      if (is.null(series)) {
        series <- rolledContract(genericCode, 
                                 rollOn = 1, 
                                 contractNumber = contract)
        strat_cache[[key]] <<- series
      }
      
      # first dates strictly anterior to "dates"
      val1 <- unlist(lapply(dates, function(d){
        i <- match(TRUE, series$Date>=d)
        if (i>1) {
          series$Value[i-1]
        } else {
          0
        }
      }))
      val2 <- unlist(lapply(dates, function(d){
        i <- match(TRUE, series$Date>=d-window)
        if (i>1) {
          series$Value[i-1]
        } else {
          0
        }
      }))
      (val1>val2)*2-1
    }
)