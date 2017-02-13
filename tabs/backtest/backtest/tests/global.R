
sourceDir("tabs/backtest/portfolio", trace = FALSE)
sourceDir("tabs/backtest/backtest", trace = FALSE)

if (!dbExistsTable(dbDBI, "portfolios")) {
  dbSendQuery(dbDBI, "CREATE TABLE portfolios(name text primary key, assetClasses text, assets text, strategies text)")
}

blankPortfolio <- function(name){
  return (list(name=name,
               assetClasses=data.table(
                 "Class"=c("Bonds", "Equity"),
                 "Weights"=c(1.5, .6)),
               assets=data.table(
                 "Code"=c("ESTX50", "ES", "ZN", "GBL"),
                 "Contract #"=rep(1,4),
                 "Weights"=rep(1,4)
               ),
               strategies=data.table(
                 "Code"=c("ESTX50", "ES", "ZN", "GBL"),
                 "Strategy"=rep("long", 4),
                 "Weights"=rep(1,4)),
               daily=FALSE,
               rollDate=1,
               leverage=1))
}

getPortfolios <- function() {
  res <- dbSendQuery(dbDBI, "SELECT * FROM portfolios")
  portfoliosRaw <- dbFetch(res)
  if (dim(portfoliosRaw)[1]>0) {
    return (apply(portfoliosRaw, 1, function(row) {
      return (list(name=row["name"],
                   assetClasses=data.table(fromJSON(toString(row["assetClasses"]))),
                   assets=data.table(fromJSON(toString(row["assets"]))),
                   strategies=data.table(fromJSON(toString(row["strategies"])))))
    }))
  } else {
    return (lapply(1:3, function(x){
      return (blankPortfolio(paste("Portfolio",x)))
    }))
  }
}

savePortfolios <- function(portfoliosList) {
  escape <- function(x) {
    x <- gsub("([\\])", "\\\\", x)
    x <- gsub("([\"])", "\\\"", x)
    return (x)
  }
  escape2 <- function(x){escape(escape(x))}
  l <- length(portfoliosList)
  for (i in 1:l) {
    quer <- paste0("INSERT OR REPLACE INTO portfolios(name, assetClasses, assets, strategies) VALUES(",
                   "'",portfoliosList[[i]]$name,"', '",
                   escape2(toJSON(portfoliosList[[i]]$assetClasses)), "', '",
                   escape2(toJSON(portfoliosList[[i]]$assets)), "', '",
                   escape2(toJSON(portfoliosList[[i]]$strategies)), "')")
    dbSendQuery(dbDBI, quer)
  }
  pnames <- unlist(child.up(portfoliosList, "name"))
  quer <- paste0("DELETE FROM portfolios WHERE name NOT IN (",
                 paste0(paste0("\"", pnames, "\""), collapse = ", "),
                 ")")
  dbSendQuery(dbDBI, quer)
}


