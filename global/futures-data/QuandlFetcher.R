
QuandlFetcher <- R6Class( "QuandlFetcher",
  
  public = list(
    
    initialize = function (tickers, rates, params) {
      
      private$dbDBI = Db$new()$singleton$localDBI
      
      if (missing(params) || is.null(params$quandl_api_key)) {
        stop("Missing Quandl API key")
      }
      self$tickers <- tickers
      self$rates <- rates
      self$params <- params
      db.codes <- c("CME", "EUREX", "SGX", "OSE", "ASX", "MX", "ICE", "HKEX", "LIFFE")
      urls <- paste0("https://www.quandl.com/api/v3/databases/",
                              db.codes,
                              "/codes?api_key=",
                              params$quandl_api_key)
      names(urls) <- db.codes
      self$urls <- urls
      self$fnames <- paste0("local/", db.codes, "-datasets-codes.csv")
      names(self$fnames) <- db.codes
      self$status <- "Fetching rates data"
    },
    
    fetchOne = function () {
      
      if (!is.data.table(self$ratesToFetch)) {
        today <- strftime(Sys.Date(),"%Y%m%d")
        res <-  dbSendQuery(private$dbDBI, 
                            paste0("select shortName, lastFetch from rates_data where lastFetch='", today, "'"))
        notfetch <- fetch(res, n=-1)
        dbClearResult(res)
        self$ratesToFetch <- self$rates %>%
          filter(!(shortName %in% notfetch$shortName)) %>%
          mutate(toFetch = 1)
        
      } else if(sum(self$ratesToFetch$toFetch)>0) {
        
        today <- strftime(Sys.Date(),"%Y%m%d")
        tofetch <- (self$ratesToFetch %>% filter(toFetch == 1))[1,]
        print(paste("fetching", tofetch$shortName))
        d<- read.csv(text=getURI(paste0("https://www.quandl.com/api/v3/datasets/",
                                        tofetch[["Quandl Code"]],
                                        ".csv?api_key=",
                                        self$params$quandl_api_key), .opts=self$params$proxy),
                     stringsAsFactors = FALSE)
        
        names(d)[1:2] <- c("Date", "Value")
        
        d$Date <- as.Date(d$Date)
        last <- which.max(d$Date)

        lastValue <- ifelse(!is.na(d$Value), d$Value[last], NA)

        insertdata <- data.frame(
          shortName=tofetch$shortName,
          lastFetch=today,
          "data"=I(list(serialize(d, NULL))),
          stringsAsFactors = FALSE
        )
        res <- dbSendQuery(private$dbDBI, paste0("delete from rates_data where shortName=\"",tofetch$shortName,"\""))
        dbClearResult(res)
        res <- dbSendPreparedQuery(private$dbDBI, "insert into rates_data values(?,?,?)", bind.data=insertdata)
        dbClearResult(res)
        self$ratesToFetch %<>% mutate(toFetch = ifelse(shortName == tofetch$shortName, 0, toFetch))
        

      # fetch tickers list, if any still to fetch
      } else if (length(self$urls)>0) {
        self$status <- "Fetching contracts lists"
        e <- names(self$urls)[1]
        fname <- self$fnames[e]
        last <- file.info(fname)$mtime
        if (!is.na(last) && last>today()) {
          print(paste("reading",e))
          codesu <- fread(fname)
        } else {
          print(paste("downloading",e))
          u <- self$urls[1]
          temp <- tempfile()
          download.file(u, temp, mode="wb")
          codesu <- fread(unzip(temp, exdir="local"))
          unlink(temp)
        }
        self$urls <- self$urls[-1]
        colnames(codesu) <- c("Code", "Name")
        self$db.codes[[e]] <- codesu
        
      # setup contracts fetching 
      } else if (!is.data.table(self$toFetch)) {
        print("preparing fetch")
        self$status <- "Fetching contracts"
        self$db.codes <- bind_rows(self$db.codes, .id="Exchange")
        ys <- CONFIG$years
        months <- "FGHJKMNQUVXZ"
        contracts <- expand.grid(
          Year = ys,
          Month = 1:12,
          Generic.code = self$tickers)
        contracts <- data.table(contracts) %>%
          mutate( Month.code = unlist(strsplit("FGHJKMNQUVXZ",""))[Month],
                  Exp.date = as.Date(paste( Year+(Month==12), 
                                            Month %% 12 + 1, 
                                            "01", sep="-"))-1,
                  Code = paste0(Generic.code, Month.code, Year))
        contracts <- inner_join(contracts, self$db.codes, by="Code")
        
        # get previously saved contracts, fetch others if necessary
        today <- strftime(Sys.Date(),"%Y%m%d")
        
        res <-  dbSendQuery(private$dbDBI, 
                      paste0("select genericCode, completeCode from contracts where (lastFetch='", today, 
                             "' or expDate<'", today, "')"))
        notfetch <- fetch(res, n=-1)
        dbClearResult(res)
        self$toFetch <- contracts %>% 
          filter(!(Code %in% notfetch$completeCode)) %>%
          mutate(toFetch = 1)

      # fetch the next contract
      } else {
        if (sum(self$toFetch$toFetch)>0) {
          today <- strftime(Sys.Date(),"%Y%m%d")
          tofetch <- (self$toFetch %>% filter(toFetch == 1))[1,]
          print(paste("fetching", tofetch$Code))
          d<- read.csv(text=getURI(paste0("https://www.quandl.com/api/v3/datasets/",
                                          tofetch$Code,
                                          ".csv?api_key=",
                                          self$params$quandl_api_key), .opts=self$params$proxy),
                       stringsAsFactors = FALSE)
          d$Date <- as.Date(d$Date)
          last <- which.max(d$Date)
          settI <- match(TRUE, grepl("Sett", colnames(d), fixed = TRUE ))
          if (is.na(settI)) settI <- match(TRUE, grepl("Close", colnames(d), fixed = TRUE ))
          lastPrice <- if (is.na(settI)) NA else d[last, settI]

          insertdata <- data.frame(
            completeCode=tofetch$Code,
            genericCode=tofetch$Generic.code,
            expDate=strftime(tofetch$Exp.date, "%Y%m%d"),
            expMonth=tofetch$Month,
            expYear=tofetch$Year,
            lastFetch=today,
            "data"=I(list(serialize(d, NULL))),
            "lastPrice"=lastPrice,
            stringsAsFactors = FALSE
          )
          res <- dbSendQuery(private$dbDBI, paste0("delete from contracts where completeCode=\"",tofetch$Code,"\""))
          dbClearResult(res)
          res <- dbSendPreparedQuery(private$dbDBI, "insert into contracts values(?,?,?,?,?,?,?,?)", bind.data=insertdata)
          dbClearResult(res)
          self$toFetch %<>% mutate(toFetch = ifelse(Code == tofetch$Code, 0, toFetch))
        } else {
          self$status <- "Complete."
        }
      }
      TRUE
    },
    
    complete = function() {
      if (is.data.table(self$toFetch)) {
        (1-ifelse(length(self$toFetch$toFetch)>0,
                  sum(self$toFetch$toFetch)/length(self$toFetch$toFetch),
                  0))
      } else if (is.data.table(self$ratesToFetch$toFetch) && sum(self$ratesToFetch$toFetch)>0) {
        (1-ifelse(length(self$ratesToFetch$toFetch)>0,
                  sum(self$ratesToFetch$toFetch)/length(self$ratesToFetch$toFetch),
                  0))*0.99
      } else {
        ((1 - length(self$urls)/9)*0.99)
      }
    },
    
    urls = c(),
    fnames = c(),
    db.codes = list(),
    toFetch = FALSE,
    ratesToFetch = FALSE,
    tickers = list(),
    rates = list(),
    params = list(),
    status = "Ready."
  ),
  
  private = list(
    dbDBI = NULL
  )
)