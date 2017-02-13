
# class used to compute performance analytics

Performance <- R6Class("Performance",

  public = list(
    metrics = c(),
    range = NULL,
    series = NULL,
    retOption = "daily"
  ),
                             
  active = list(
    
    results = function () {
      
      if (is.null(self$series)) {
        NULL
      } else {
        series <- self$series
        if (!is.null(self$range)) {
          series %<>% filter(Date >= self$range[1] & Date <= self$range[2])
        }
        
        # calculate main metrics
        pstats <- lapply(colnames(series)[-(1:2)], function(p){
          seriesOne <- series[,c("Date", "period",p),with=FALSE]
          key <- digest(list(seriesOne, self$retOption))
          if (!is.null(private$cache_metrics[[key]])) {
            private$cache_metrics[[key]]
          } else {
            colnames(seriesOne) <- c("Date", "period", "Value")
            if (self$retOption == "rolling") {
              seriesOne %<>% 
                arrange(Date) %>%
                mutate(ret = exp(c(rep(0, 10), diff(log(Value), lag=10)))^(1/10)-1)
            } else if (self$retOption == "period") {
              seriesOne %<>% 
                arrange(Date) %>%
                filter(period != lag(period)) %>%
                mutate(ret = exp(c(0, diff(log(Value))))-1)
            } else {
              seriesOne %<>% 
                arrange(Date) %>%
                mutate(ret = exp(c(0, diff(log(Value))))-1)
            }
            returns <- xts(seriesOne %>% select(ret), order.by=seriesOne$Date)
            annret <- Return.annualized(returns)
            stdev <- StdDev.annualized(returns)
            skew <- skewness(returns$ret)
            kurt <- kurtosis(returns$ret)
            varmod <- VaR(returns, method = "modified")
            res <- data.table(
              Order = 1:9,
              Metric = c(
                "Annual returns",
                "Annual StdDev",
                "1/StdDev",
                "StdDev Sharpe",
                "Sortino ratio",
                "Skewness",
                "Kurtosis",
                "Skewness / Kurtosis",
                "VaR"
              ),
              Value = c(
                sprintf("%1.1f%%", 100*annret),
                sprintf("%1.1f%%", 100*stdev),
                sprintf("%1.1f", 1/stdev),
                sprintf("%1.2f", annret/stdev),
                sprintf("%1.2f", SortinoRatio(returns)),
                sprintf("%1.2f", skew),
                sprintf("%1.2f", kurt),
                sprintf("%1.2f", skew/kurt),
                sprintf("%1.1f%%", 100*varmod)
              )
            ) %>% mutate(Portfolio=p)
            private$cache_metrics[[key]]<-res
            res
          }
        }) %>% bind_rows() %>% spread(Portfolio, Value)
        
        # calculate annual returns
        if ("annual" %in% self$metrics) {
          pstats <- bind_rows(pstats,
            series %<>%
              gather(Portfolio, Value, -(1:2)) %>%
              mutate(Year = year(Date)) %>%
              group_by(Portfolio) %>%
              arrange(Date) %>%
              fill(Value) %>%
              filter(Year != lag(Year) | row_number(Date) == 1 | row_number(Date) == n()) %>%
              mutate(ret = c(exp(diff(log(Value)))-1,NA)) %>%
              ungroup() %>%
              filter(!is.na(ret)) %>%
              transmute(Portfolio = Portfolio, 
                        Metric = as.character(Year), 
                        Value = sprintf("%1.1f%%", 100*ret)) %>%
              spread(Portfolio, Value) %>%
              arrange(Metric) %>%
              mutate(Order=9+row_number(Metric)))
        }
        
        pstats %>%
          arrange(Order) %>% select(-Order)
      }
    }
  ),
  
  private = list(
    cache_metrics = list()
  )
)

