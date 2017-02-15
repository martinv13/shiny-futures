
Db <- R6Class("Db",
  
  public = list(
    localDBI = NULL,
    localDPL = NULL,
    portableDBI = NULL,
    portableDPL = NULL,
    
    init = function (localf, portablef) {
      
      sqlite    <- dbDriver("SQLite")
      
      dir.create(dirname(localf), showWarnings = FALSE)
      
      self$localDPL <- src_sqlite(localf, create = TRUE)
      self$localDBI <- dbConnect(sqlite, localf)
      
      dir.create(dirname(portablef), showWarnings = FALSE)
      
      self$portableDPL <- src_sqlite(portablef, create = TRUE)
      self$portableDBI <- dbConnect(sqlite, portablef)
      
      if (!dbExistsTable(self$localDBI, "contracts")) {
        res <- dbSendQuery(self$localDBI, "CREATE TABLE contracts(completeCode text, genericCode text, expDate text, expMonth numeric, expYear numeric, lastFetch text,  data blob,  lastPrice real)")
        dbClearResult(res)
      }
      
      if (!dbExistsTable(self$portableDBI, "tickers")) {
        res <- dbSendQuery(self$portableDBI, "CREATE TABLE tickers (Name TEXT, Type TEXT, \"IB Ticker\" TEXT PRIMARY KEY, \"Quandl Code\" TEXT, \"Multiplier\" REAL, \"Currency\" TEXT, \"Margin\" REAL, \"RefNum\" TEXT)")
        dbClearResult(res)
      }
      
      if (!dbExistsTable(self$portableDBI, "rates")) {
        res <- dbSendQuery(self$portableDBI, "CREATE TABLE rates (shortName TEXT primary key, Name TEXT, \"Quandl Code\" TEXT, Type TEXT, Country TEXT, Term TEXT)")
        dbClearResult(res)
      }
      
      if (!dbExistsTable(self$localDBI, "rates_data")) {
        res <- dbSendQuery(self$localDBI, "CREATE TABLE rates_data(shortName text,  lastFetch text, data blob)")
        dbClearResult(res)
      }
      
      if (!dbExistsTable(self$portableDBI, "fx")) {
        res <- dbSendQuery(self$portableDBI, "CREATE TABLE fx (shortName text primary key, Name TEXT, \"Quandl Code\" TEXT, codeFrom TEXT, codeTo TEXT)")
        dbClearResult(res)
      }
      
      if (!dbExistsTable(self$localDBI, "fx_data")) {
        res <- dbSendQuery(self$localDBI, "CREATE TABLE fx_data(shortName text, lastFetch text, data blob)")
        dbClearResult(res)
      }
      
      if (!dbExistsTable(self$localDBI, "keyval")) {
        res <- dbSendQuery(self$localDBI, "CREATE TABLE keyval (key TEXT PRIMARY KEY, value TEXT)")
        dbClearResult(res)
      }
      
      if (!dbExistsTable(self$localDBI, "series")) {
        res <- dbSendQuery(self$localDBI, "CREATE TABLE series (key TEXT PRIMARY KEY, data BLOB)")
        dbClearResult(res)
      }
      
      if (!dbExistsTable(self$portableDBI, "portfolios")) {
        res <- dbSendQuery(self$portableDBI, "CREATE TABLE portfolios(name text primary key, assetClasses text, assets text, strategies text)")
        dbClearResult(res)
      }
      
      if (!dbExistsTable(self$localDBI, "strats_cache")) {
        res <- dbSendQuery(self$localDBI, "CREATE TABLE strats_cache (Date TEXT, genericCode TEXT, call TEXT, signature TEXT, Position DOUBLE, v1 DOUBLE, v2 DOUBLE, v3 DOUBLE, v4 DOUBLE, v5 DOUBLE)")
        dbClearResult(res)
      }
      
      
      self
    },
    
    writeKey = function (key, value) {
      res <- dbSendQuery(self$localDBI, paste0("INSERT OR REPLACE INTO keyval(key, value) VALUES(\"",key,"\",\"",value,"\")"))
      dbClearResult(res)
    },
    
    readKey = function (key, default=NULL) {
      quer <- dbSendQuery(self$localDBI, paste0("SELECT * FROM keyval WHERE key=\"",key,"\""))
      res <- fetch(quer)
      dbClearResult(quer)
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
    },
    
    writeSeries = function (key, value) {
      insertdata <- data.frame(
        key=key,
        data=I(list(serialize(value, NULL))),
        stringsAsFactors = FALSE
      )
      res <- dbSendPreparedQuery(self$localDBI, "insert or replace into series(key, data) values(?,?)", bind.data=insertdata)
      dbClearResult(quer)
    },
    
    readSeries = function (key) {
      quer <- dbSendQuery(self$localDBI, paste0("SELECT * FROM series WHERE key=\"",key,"\""))
      res <- fetch(quer)
      dbClearResult(quer)
      data <- NULL
      if (!is.null(res) && !is.null(res$key) && length(res$key)>0){
        data <- unserialize(res$data[[1]])
      }
      data
    }
    
  ),
  
  active = list(
    singleton = function() {
      if (is.null(private$senv$sgt)) {
        private$senv$sgt <- Db$new()
      }
      private$senv$sgt
    }),
  
  private = list(senv = new.env())
)

