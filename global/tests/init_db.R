
Db <- R6Class("Db",
  
  public = list(
    dbDBI = NULL,
    dbDPL = NULL,
    
    initialize = function (fname) {
      sqlite    <- dbDriver("SQLite")
      dbDBI <- dbConnect(sqlite, fname)
      
    }
    
  ),
  private = list()
)


updateTickers <- function(tickers) {
  if (!is.null(tickers)) {
    tickers %<>% select(Name, `IB Ticker`, `Quandl Code`, Type, Multiplier, Currency)
    if(length(tickers$Name)>0) {
      quer <- paste0("DELETE FROM tickers WHERE \"IB Ticker\" NOT IN (",
                     paste(paste0("\"", tickers$`IB Ticker`, "\""), collapse = ","),
                     ")")
      dbSendQuery(dbDBI, quer)
    }
    for (i in 1:length(tickers$Name)) {
      if (!is.na(tickers$`IB Ticker`[i])) {
        quer <- paste0("INSERT OR REPLACE INTO tickers(",
                       paste0(paste0("\"", names(tickers), "\""), collapse = ","),
                       ") VALUES(",
                       paste0(ifelse(sapply(tickers[1,],is.numeric), tickers[i,], paste0("\"", tickers[i,], "\"")), collapse = ","),
                       ")")
      } else {
        quer <- paste0("INSERT OR REPLACE INTO tickers(Name, \"IB Ticker\", \"Quandl Code\",",
                       "Type, Multiplier, Currency) values(\"new\",\"new\", \"\", \"\", 0, \"USD\")")
      }
      dbSendQuery(dbDBI, quer)
    }
  }
}

if (!dbExistsTable(dbDBI, "contracts")) {
  dbSendQuery(dbDBI, "CREATE TABLE contracts(completeCode text, genericCode text, expDate text, expMonth numeric, expYear numeric,  lastFetch text,  data blob)")
}

if (!dbExistsTable(dbDBI, "tickers")) {
  dbSendQuery(dbDBI, "CREATE TABLE tickers (\"Name\" TEXT, Type TEXT, \"IB Ticker\" TEXT PRIMARY KEY, \"Quandl Code\" TEXT, \"Multiplier\" INTEGER, \"Currency\" TEXT)")
  tickers <- fread(CONFIG$tickers, sep=";", dec=",", stringsAsFactors = FALSE)
  updateTickers(tickers)
}

dbDPL <- src_sqlite(CONFIG$db)

tickers_db <- tbl(dbDPL, "tickers")
contracts_db <- tbl(dbDPL, "contracts")

getTickers <- function() {
  if (!is.null(tickers_db) && !is.null(contracts_db)) {
    fetch_data <- contracts_db %>% group_by(genericCode) %>%
      summarise(nbContracts = n(),
                lastFetch = max(lastFetch),
                start = min(expDate))
    return (tickers_db %>%
              left_join(fetch_data,
                        by=c("genericCode"="Quandl Code")) %>% collect() %>% select(-genericCode) )
  }
}

gl_tickers <- getTickers()

if (!dbExistsTable(dbDBI, "keyval")) {
  dbSendQuery(dbDBI, "CREATE TABLE keyval (key TEXT PRIMARY KEY, value TEXT)")
}

writeKey <- function (key, value) {
  dbSendQuery(dbDBI, paste0("INSERT OR REPLACE INTO keyval(key, value) VALUES(\"",key,"\",\"",value,"\")"))
}

readKey <- function (key, default=NULL) {
  res <- fetch(dbSendQuery(dbDBI, paste0("SELECT * FROM keyval WHERE key=\"",key,"\"")))
  if (!is.null(res) && !is.null(res$key) && length(res$key)>0){
    convert <- suppressWarnings(as.numeric(res$value[1], t))
    if (!is.na(convert)) {
      return (convert)
    } else {
      if (res$value[1] == "FALSE") {
        return (FALSE)
      } else if (res$value[1] == "TRUE") {
        return (TRUE)
      } else {
        return (res$value[1])        
      }
    }
  }
  if (!is.null(default)) {
    return (default)
  } else {
    return (NULL)
  }
}

if (!dbExistsTable(dbDBI, "series")) {
  dbSendQuery(dbDBI, "CREATE TABLE series (key TEXT PRIMARY KEY, data BLOB)")
}

writeSeries <- function (key, value) {
  insertdata <- data.frame(
    key=key,
    data=I(list(serialize(value, NULL))),
    stringsAsFactors = FALSE
  )
  dbSendPreparedQuery(dbDBI, "insert or replace into series(key, data) values(?,?)", bind.data=insertdata)
}

readSeries <- function (key) {
  res <- fetch(dbSendQuery(dbDBI, paste0("SELECT * FROM series WHERE key=\"",key,"\"")))
  data <- NULL
  if (!is.null(res) && !is.null(res$key) && length(res$key)>0){
    data <- unserialize(res$data[[1]])
  }
  return (data)
}

deleteSeries <- function () {
#  dbSendQuery
}

