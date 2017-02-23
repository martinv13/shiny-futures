

s <- Contract$new("CME/ES")$monthlyRoll()$series

tb <- table(s$Date)
tb[tb>1]

View(s)

View(Contract$new("")$single("CME/ESU2016")$series)

self <- Portfolio$new("test")

p$backtest()

View(p$series)



private = list(
  name_p = "testPortfolio",
  fd = futuresData,
  db = db,
  rollDates = 1,
  classes_df = data.table(
    "Class" = c("Equity","Bonds"),
    "Weight" = c(1,1)),
  assets_df = data.table(
    "Code" = c("ESTX50","GBL"),
    "Contract #" = c(1,1),
    "Weight" = c(1,1)),
  strats_df = data.table(
    "Code" = c("ESTX50","ESTX50","ESTX50","GBL"),
    "Strategy" = c("trendS(30-90-200,1,1,0,0,1)","carryMA(12,10)", "long","long"),
    "Weight" = c(1,2,1,1)),
  results = NULL,
  lastBacktest = NULL,
  lastBacktestFreq = NULL,
  series_p = NULL
)
rollOn <- 1
self <- list(notional=100, rounded=FALSE)

