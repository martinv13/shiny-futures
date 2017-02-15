# portfolio with a single asset and asset strategy
portfolio <- list(
  strategies=data.table(Code="ZN", Weights=1, Strategy="long"),
  assets=data.table(Code="ZN", Weights=1, "Contract #"=1),
  assetClasses=data.table(Class="Bonds", Weights=1))

portfolio <- blankPortfolio("test")

portfolio$daily <- FALSE
r<-backtest(portfolio)
periodReturns<-r$returns %>% arrange(Date) %>% mutate(value=exp(cumsum(log(Returns+1))))

portfolio$daily <- TRUE
r<-backtest(portfolio)
dailyReturns<-r$returns %>% arrange(Date) %>% mutate(value=exp(cumsum(log(Returns+1))))

r<-getContractData(list(genericCode="CME/TY", 
                        rollDate=32,
                        rollContract=1,
                        contractType="roll"))
r$series$Value<-r$series$Value/r$series$Value[1]

series <- merge(xts(dailyReturns$value, order.by = dailyReturns$Date),
                xts(periodReturns$value, order.by = periodReturns$Date),
                xts(r$series$Value, order.by = r$series$Date))
colnames(series) <- c("daily", "period", "ref")
series <- na.approx(series)
dygraph(series) %>%
  dyRangeSelector() %>% dyOptions(logscale=TRUE) %>% dyLegend(show="follow")

data(data_stock1)
data_stock1$chartData1$value2 <- as.numeric(data_stock1$chartData1$value) + 10
data_stock1$chartData2$value2 <- as.numeric(data_stock1$chartData2$value) + 10
data_stock1$chartData3$value2 <- as.numeric(data_stock1$chartData3$value) + 10
data_stock1$chartData4$value2 <- as.numeric(data_stock1$chartData4$value) + 10

data_stock1$chartData1$value3 <- as.numeric(data_stock1$chartData1$value) - 10
data_stock1$chartData2$value3 <- as.numeric(data_stock1$chartData2$value) - 10
data_stock1$chartData3$value3 <- as.numeric(data_stock1$chartData3$value) - 10
data_stock1$chartData4$value3 <- as.numeric(data_stock1$chartData4$value) - 10

amStockMultiSet(data = data_stock1)
amStockMultiSet(data = data_stock1, panelColumn = c(1,2,1,1))

amStockMultiSet(data = data_stock1, panelColumn = c(1,2,3,4))

ZoomButton <- data.frame(Unit = c("DD", "DD", "MAX"), multiple = c(1, 10 ,1),
                         label = c("Day","10 days", "MAX"))
ZoomButtonPosition <- "bottom"
amStockMultiSet(data = data_stock1, panelColumn = c(1,2,1,1), ZoomButton = ZoomButton,
                ZoomButtonPosition = "top")

amStockMultiSet(data = data_stock1, precision = 2)

amStockMultiSet(data = data_stock1, panelColumn = c(1,2,1,1), percentHeightPanel = c(3,1))




