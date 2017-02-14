print("loading app...")

source("global/dependencies.R")

source("config.R")

# utils functions
source("global/utils.R")

sourceDir("global/futures-data", trace = FALSE)


# init db
db <- Db$new()$singleton$init(localf="local/contracts_data.sqlite", portablef="portable.sqlite")

# data source R6 class
futuresData <- FuturesData$new()$singleton
futuresData$load()
ratesData <- RatesData$new()$singleton
ratesData$load()
fXData <- FXData$new()$singleton
fXData$load()


if (dim(futuresData$tickers)[1]==0) {
  futuresData$tickers <- read.csv("global/tickers.csv", sep=";", dec=",", stringsAsFactors = FALSE)
}

# loading modules 
sourceDir("modules/plotSeries", trace = FALSE)
sourceDir("modules/plotPortfolios", trace = FALSE)
sourceDir("modules/selContracts", trace = FALSE)
sourceDir("modules/portfolio", trace = FALSE)
sourceDir("modules/performanceStats", trace = FALSE)


# loading tabs
sourceDir("tabs/update", trace = FALSE)
sourceDir("tabs/overview", trace = FALSE)
sourceDir("tabs/explore", trace = FALSE)
sourceDir("tabs/backtest", trace = FALSE)
