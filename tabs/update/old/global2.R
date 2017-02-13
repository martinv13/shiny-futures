
QuandlFetcher <- R6Class( "QuandlFetcher",
  public = list(
    initialize = function (tickers, params) {
      if (missing(params) || is.null(params$quandl_api_key)) {
        stop("Missing Quandl API key")
      }
      self$tickers <- tickers
      self$params <- params
      db.codes <- c("CME", "EUREX", "SGX", "OSE", "ASX", "MX")
      urls <- paste0("https://www.quandl.com/api/v3/databases/",
                              db.codes,
                              "/codes?api_key=",
                              params$quandl_api_key)
      names(urls) <- db.codes
      self$urls <- urls
      self$status <- "Fetching tickers lists"
    },
    
    fetchOne = function () {
      
      # fetch tickers listing, if any still to fetch
      if (length(self$urls)>0) {
        e <- names(self$urls)[1]
        print(paste("downloading",e))
        u <- self$urls[1]
        self$urls <- self$urls[-1]
        temp <- tempfile()
        download.file(u, temp, mode="wb")
        codesu <- fread(unzip(temp, exdir="tickers-data"))
        unlink(temp)
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
        notfetch <- fetch(
          dbSendQuery(dbDBI, 
                      paste0("select genericCode, completeCode from contracts where (lastFetch='", today, 
                             "' or expDate<'", today, "')")), n=-1)
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
          insertdata <- data.frame(
            completeCode=tofetch$Code,
            genericCode=tofetch$Generic.code,
            expDate=strftime(tofetch$Exp.date, "%Y%m%d"),
            expMonth=tofetch$Month,
            expYear=tofetch$Year,
            lastFetch=today,
            data=I(list(serialize(d, NULL))),
            stringsAsFactors = FALSE
          )
          dbSendQuery(dbDBI, paste0("delete from contracts where completeCode=\"",tofetch$Code,"\""))
          dbSendPreparedQuery(dbDBI, "insert into contracts values(?,?,?,?,?,?,?)", bind.data=insertdata)
          self$toFetch %<>% mutate(toFetch = ifelse(Code == tofetch$Code, 0, toFetch))
        } else {
          self$status <- "Complete."
        }
      }
      TRUE
    },
    complete = function() {
      if (is.data.table(self$toFetch)) {
        return (1-ifelse(length(self$toFetch$toFetch)>0,
                         sum(self$toFetch$toFetch)/length(self$toFetch$toFetch),
                         0))
      } else {
        return ((1 - length(self$urls)/6)*0.99)
      }
    },
    urls = c(),
    db.codes = list(),
    toFetch = FALSE,
    tickers = list(),
    params = list(),
    status = "Ready."
  )
)