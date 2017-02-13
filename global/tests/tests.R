

futuresData$loadData()

futuresData$getTickers()

contracts <- Contracts$new()


strategies[["test2"]] <- function(genericCode, dates) rep(2, length(dates))

test <- Contracts$new("CME/NG")$
  monthlyRoll(strategy="trend", stratParams=list(30,1))$
  data()

strategies$list()
strategies$arguments("trend")

test$data()%>%
  select(Date, Value) %>% dygraph() %>% dyRangeSelector() %>% dyOptions(logscale=TRUE)

contract <- "CME/ES"
series <- list( "short" = Contracts$new(contract)$monthlyRoll(strategy = "long"),
                "trend-30" = Contracts$new(contract)$monthlyRoll(strategy = "trend(30,1)"),
                "trend-90" = Contracts$new(contract)$monthlyRoll(strategy = "trend(90,1)"))

series %>%
  lapply(function(s) s$series()) %>%
  bind_rows(.id="series") %>%
  select(Date, Value, series) %>%
  spread(series, Value) %>%
  dygraph() %>% dyRangeSelector()



