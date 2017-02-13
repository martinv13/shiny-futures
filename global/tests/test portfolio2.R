

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
  fd = NULL,
  db = NULL,
  rollDates = 1,
  classes_df = data.table(
    "Class" = c("Equity"),
    "Weight" = 1),
  assets_df = data.table(
    "Code" = c("ESTX50"),
    "Contract #" = 1,
    "Weight" = 1),
  strats_df = data.table(
    "Code" = c("ESTX50","ESTX50","ESTX50"),
    "Strategy" = c("trendS(30-90-200,1,1,0,0,1)","carryMA(12,10)", "long"),
    "Weight" = 1),
  results = NULL,
  lastBacktest = NULL,
  lastBacktestFreq = NULL,
  series_p = NULL
)
rollOn <- 1

