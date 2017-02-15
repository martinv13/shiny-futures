

eur <- Contract$new("CME/ES")$monthlyRoll()

dygraph(eur$series %>% select(Date, Value) ) %>% dyRangeSelector()

dates <- eur$series %>% filter(period != lag(period))

strat <- Strategy$new("trend(90,0.8,0.01,1)")

r <- strat$fun("CME/ES", dates$Date)

r %<>% left_join(eur$series %>% select(Date, Value), by="Date")

r %>% mutate(Slope = v2*100, zero=0, Residual=v1) %>%
  select(Date, Value, Residual, Slope, zero) %>% 
  dygraph() %>%
#  dySeries(name="Position", stepPlot = TRUE, axis="y2") %>%
  dySeries(name="Residual", axis="y2") %>%
  dySeries(name="Slope", axis="y2") %>%
  dySeries(name="zero", axis="y2") %>%
  dyRangeSelector()

bind_cols(data.table(dates),data.table(slopes), data.table(breakouts)) %>%
  setNames(c("Date",paste(rep(c("slp", "brk"), each=3), rep(1:3,2)))) %>%
  transmute(Date = Date,
            sum.breakout = `brk 1`+`brk 2`+`brk 3`,
            sign.breakout = (((`brk 1`>0)+(`brk 2`>0)+(`brk 3`>0))*2-3)/15) %>%
  left_join(series %>% select(Date, Value), by="Date") %>%
  dygraph() %>% 
  dySeries(name="Value", axis="y2") %>%
  dyRangeSelector()


b<-Contract$new("CME/EC")$monthlyRoll(strategy="strend(90,.5,1)")

eur$series %>%
  select(Date, Value) %>%
  dygraph()

code <- "TT"
strat <- Strategy$new("strend(200,0.5,1)")
Contract$new("")

private = list(
  name_p = "testPortfolio",
  
  fd = futuresData,
  db = db,
  
  classes_df = data.table(
    "Class" = c("Equity", "Energy"),
    "Weight" = c(1.5, .6)),
  
  assets_df = data.table(
    "Code" = c("ESTX50", "ES", "QM", "QG"),
    "Contract #" = rep(1,4),
    "Weight" = rep(1,4)),
  
  strats_df = data.table(
    "Code" = c("ESTX50", "ES", "QM", "QG"),
    "Strategy" = rep("trendS(30-90-200,1,0,0,0,1)", 4),
    "Weight" = rep(1,4)),
  
  results = NULL,
  
  lastBacktest = NULL,
  lastBacktestFreq = NULL,
  
  series_p = NULL,
  positions_p = NULL
  
)

