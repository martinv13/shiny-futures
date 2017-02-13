
# data source

FuturesData <- R6Class("FuturesData",
                      
  cloneable = FALSE,
  
  public = list(
    
    # initialize DataSource with a db connection object
    initialize = function () {
      private$db <- Db$new()$singleton
    },
    
    # load data from db
    load = function() {
      
      private$fetchTickers()
      
      if (dim(self$tickers)[1]==0) {
        self$tickers <- fread("./global/tickers.csv")
      }
      
#      key <- digest
      
      selectedContracts <- self$tickers$`Quandl Code`
      
      # extract data from db
      raw.data <- tbl(private$db$localDPL, "contracts")
      if (length(selectedContracts) == 1) {
        raw.data %<>% filter(genericCode == selectedContracts)
      } else {
        raw.data %<>% filter(genericCode %in% selectedContracts)
      }
      raw.data %<>%
        collect() %>%
        mutate(expDate=as.Date(expDate, format="%Y%m%d"),
               genericCode2=genericCode) %>%
        separate(genericCode2, c("exchange", "code"), sep="/")
      
      if (length(raw.data$expDate)==0) {
        return (FALSE)
      }
      
      extr.data <- raw.data
      extr.data$data <- lapply(extr.data$data, function(x) {
        unserialize(x)
      })
      extr.data$colNames <- unlist(lapply(extr.data$data, function(x){
        paste(colnames(x), collapse="_")
      }))
      extr.data %<>%
        group_by(colNames) %>%
        do({
          completeCodes <- .$completeCode
          extracted <- .$data %>%
            setNames(completeCodes) %>%
            bind_rows(.id="completeCode") %>%
            notNull(to="Date", "Date") %>%
            notNull(to="Settle", 
                    "Settle", 
                    "Settlement.Price",
                    "Settlement Price",
                    "Sett.Price",
                    "Previous Settlement",
                    "Previous.Settlement",
                    "Prev..Day.Settlement.Price",
                    "Close",
                    "Column.1") %>%
            notNull(to="OI",
                    "Prev. Day Open Interest",
                    "Open Interest",
                    "Open Interest (OI)",
                    "Open.Interest",
                    "Prev..Day.Open.Interest",
                    "Open.Interest..OI.",
                    "Prev. Day Open Interest",
                    "Previous.Day.Open.Interest") %>%
            select(completeCode, Date, Settle, OI)
          if (sum(is.na(extracted$Settle))>0.5*length(extracted$Settle)) {
            print(paste("Settle data not found in ", paste(completeCodes, collapse=", "), "."))
            print(.$colNames[1])
            print(" ")
          }
          if (sum(is.na(extracted$OI))>0.5*length(extracted$OI)) {
            print(paste("OI data not found in ", paste(completeCodes, collapse=", "), "."))
            print(.$colNames[1])
            print(" ")
          }
          extracted
        }) %>% ungroup() %>% select(-colNames) %>% data.table()
      
      # append expDate
      self$data <- left_join(extr.data,
                raw.data %>% data.table() %>%
                  select(genericCode, completeCode, expDate),
                by="completeCode") %>%
        group_by(completeCode) %>%
        arrange(Date) %>%
        filter(!is.na(Settle) & Settle>0) %>%
        mutate(maxDate = max(Date),
               logReturns = c(0, diff(log(Settle)))) %>%
        filter(abs(logReturns) < quantile(abs(logReturns), 0.90)*10) %>%
        ungroup()
      
      private$publish()
      NULL
    },
    
    # list of complete codes
    completeCodesReactive = function (code) {
      reactive({
        if(!is.null(self$data)){
          as.list(self$data %>% ungroup() %>% filter(genericCode == code) %>%
                    distinct(completeCode, .keep_all = TRUE) %>% 
                    arrange(desc(expDate)) %>%
                    select(completeCode))$completeCode
        } else {
          list()
        }
      })
    },
    
    subscribeDataChange = function (fun) {
      private$subscribed = c(private$subscribed, fun)
    },

    correlations = function (range=NULL) {
      # filter tickers with a numeric RefNum
      codes <- self$tickers
      codes <- codes[,`:=`("RefNum"=as.numeric(RefNum),
                           "genericCode"=`Quandl Code`)][!is.na(RefNum),
                                                         .(genericCode, 
                                                            Type,
                                                           `IB Ticker`,
                                                            RefNum,
                                                            start)]
      # join contracts data and codes
      data <- self$data
      data <- data[codes[,.(genericCode, Type, RefNum, `IB Ticker`)],
                   on="genericCode",
                   nomatch=0]
      # filter if range is provided
      if (!is.null(range)) {
        range <- as.Date(range)
        if (sum(is.na(range))==0) {
          data <- data[Date >= range[1] & Date <= range[2]]
        }
      }
      # extract the RefNum-th contract for each date
      data <- data[data[order(expDate), .I[min(.N, RefNum)], keyby=.(genericCode,Date)]$V1]
      
      stdDev <- codes
      # compute stddev for each contract
      dataStdDev <- data[,.(StdDev=as.numeric(StdDev.annualized(
        exp(logReturns)-1, scale=252))), 
        by=genericCode] %>%
        .[,`1/V`:=ifelse(is.na(StdDev),NA,1/StdDev)] %>%
        .[,`1/V`:=`1/V`/mean(`1/V`, na.rm=TRUE)]

      stdDev %<>% left_join(dataStdDev, by="genericCode")

      dataStdDev <- codes[dataStdDev, on="genericCode"]
      dataStdDev[, assetWeight := `1/V`/sum(`1/V`, na.rm=TRUE), by=Type]
      stdDevClass <- data[dataStdDev, on="genericCode"] %>%
        .[, classLogRet := log(sum((exp(logReturns)-1)*assetWeight, na.rm=TRUE)+1), by=.(Date,Type)] %>%
        .[, .(classWeight=1/as.numeric(StdDev.annualized(
                      exp(classLogRet)-1, scale=252))),
          by=Type]
      dataStdDev <- stdDevClass[dataStdDev, on="Type"]
      dataStdDev[,totalWeight:=classWeight*assetWeight]
      dataStdDev[,`1/V Class`:=totalWeight/mean(totalWeight)]
      stdDev %<>% left_join(dataStdDev[,.(`IB Ticker`,`1/V Class`)], by="IB Ticker")

      # compute indexes to reorder cols
      norder <- codes[genericCode%in%data$genericCode] %>%
        .[order(Type, `IB Ticker`), o:=1:.N] %>%
        .[order(`IB Ticker`)]
      norder <- order(norder$o)
      
      # compute correlations
      dataCorrel <- data %>%
        group_by(genericCode) %>%
        arrange(Date) %>%
        mutate(logValue = cumsum(logReturns)) %>%
        select(Date, `IB Ticker`, logValue) %>%
        spread(`IB Ticker`, logValue) %>%
        fill(-Date) %>%
        gather(`IB Ticker`, logValue, -Date) %>%
        filter(!is.na(logValue)) %>%
        group_by(`IB Ticker`) %>%
        arrange(Date) %>%
        mutate(rollReturns = c(rep(NA,10),exp(diff(logValue, lag=10))-1)) %>%
        filter(!is.na(rollReturns)) %>%
        left_join(codes %>% select(`IB Ticker`, Type), by="IB Ticker") %>%
        arrange(Type, `IB Ticker`) %>%
        select(Date, `IB Ticker`, rollReturns) %>%
        spread(`IB Ticker`, rollReturns) %>%
        select(-Date) %>%
        select(norder)
      
      corr <- cor(dataCorrel, use="complete.obs")
    
      # compute erc portfolio
      covv <- cov(dataCorrel, use="complete.obs")

      n <- dim(covv)[1]
      o <- nlminb(rep(1,n-1),
                  function(x){
                    x <- c(1, x)
                    x <- x/mean(x)
                    rc <- x*(covv %*% x)
                    sum((rc-mean(rc))^2)
                  },
                  lower = rep(0, n-1),
                  control=list(iter.max=1000,
                               eval.max=2000))
      print(o)
      erc<-c(1,o$par)
      erc <- data.table(`IB Ticker`= rownames(covv),
                        ERC = erc/mean(erc))
      stdDev %<>% left_join(erc, by="IB Ticker")

      list(stdDev=stdDev %>% arrange(Type) %>% select(start, Type,`IB Ticker`,RefNum,StdDev,`1/V`,`1/V Class`,ERC),
           correl = corr)
    },
    
    # store contracts data
    data = NULL
  ),
  
  active = list(    
    
    # returns a reactive data.frame with contracts properties
    tickersReactive = function () {
      reactive({
        private$reactives$tickers
      })
    },
    
    # formatted genericCodes
    genericCodesReactive = function (){
      reactive({
        do.call(c, ((private$reactives$tickers %>%
                       filter(Name != "new") %>%
                       group_by(Type) %>%
                       do(vals=data.frame(.)))$vals %>%
                      lapply(function(x){
                        temp <- list()
                        temp[[x$Type[1]]] <- x$Quandl.Code
                        names(temp[[x$Type[1]]]) <- paste(x$IB.Ticker, "-", x$Name)
                        temp
                      })))
      })
    },
    
    # returns tickers data
    tickers = function (ntickers = NULL) {
      if (!is.null(ntickers)) {
        oldTickers <- isolate(private$reactives$tickers)
        cols <- c("Name", "IB Ticker", "Quandl Code", "Type", "Multiplier", "Currency", "Margin", "RefNum")
        oldTickers[,old:=TRUE]
        delta <- oldTickers[,c(cols,"old"),with=FALSE] %>%
          .[ntickers[,cols,with=FALSE], on=cols] %>%
          .[is.na(old), -"old", with=FALSE]
        deleted <- oldTickers[!(`IB Ticker` %in% ntickers$`IB Ticker`)]
        delta[is.na(`IB Ticker`), `IB Ticker`:="new"]
        if (dim(deleted)[1]>0) {
          quer <- paste0("DELETE FROM tickers WHERE \"IB Ticker\" NOT IN (",
                         paste(paste0("\"", ntickers$`IB Ticker`, "\""), collapse = ","),
                         ")")
          res <- dbSendQuery(private$db$portableDBI, quer)
          dbClearResult(res)
        }
        l <- dim(delta)[1]
        if (l>0) {
          for (i in 1:l) {
            row <- delta[i,]
            row <- unlist(ifelse(is.na(row), "NULL",
                          ifelse(sapply(row, is.numeric), row, paste0("\"",row,"\""))))
            quer <- paste0("INSERT OR REPLACE INTO tickers(",
                           paste0(paste0("\"", names(delta), "\""), collapse = ","),
                           ") VALUES(",
                           paste(row, collapse = ", "),
                           ")")
            res <- dbSendQuery(private$db$portableDBI, quer)
            dbClearResult(res)
          }
        }
        private$fetchTickers()
      }
      data.table(isolate(private$reactives$tickers))
    },
    
    singleton = function() {
      if (is.null(private$senv$sgt)) {
        private$senv$sgt <- FuturesData$new()
      }
      private$senv$sgt
    }
  
  ), 
  
  private = list(
    db = NULL,
    senv = new.env(),
    codesCorrel = NULL,
    dataCorrel = NULL,
    
    reactives = reactiveValues(tickers = NULL),
    
    # fetch tickers from DB; called after update
    fetchTickers = function() {
      tickers_db <- tbl(private$db$portableDPL, "tickers")
      contracts_db <- tbl(private$db$localDPL, "contracts")
      if (!is.null(tickers_db) && !is.null(contracts_db)) {
        
        fetch_data <- contracts_db %>% 
          select(genericCode, expDate, lastPrice, lastFetch) %>%
          collect() %>% data.table() %>%
          group_by(genericCode) %>%
          mutate(nbContracts = n(),
                 lastFetch = max(lastFetch),
                 start = min(expDate)) %>%
          filter(!is.na(lastPrice) & expDate>=max(lastFetch)) %>%
          group_by(genericCode) %>%
          filter(row_number(expDate)==1) %>%
          select(genericCode, lastPrice, start, nbContracts, lastFetch)
        
        temp <- tickers_db %>% 
          collect() %>% data.table() %>%
          mutate(genericCode = `Quandl Code`) %>%
          left_join(fetch_data, by="genericCode") %>%
          select(-genericCode)
        
        chg <- data.table(
          symbol = c("EUR", "AUD", "CAD", "JPY", "GBP"),
          "IB Ticker" = c("M6E", "M6A", "MCD", "MJY", "M6B")
        ) %>% left_join(temp %>% select(`IB Ticker`, lastPrice), by="IB Ticker")
        chg <- c(chg$lastPrice %>% setNames(chg$symbol), USD=1)

        temp %<>% mutate(sizeEUR = Multiplier*lastPrice*chg[Currency]/chg["EUR"]) %>%
          select(Name,Type,`IB Ticker`,`Quandl Code`,Multiplier,Currency,Margin,RefNum,lastPrice,sizeEUR,start,nbContracts,lastFetch)
        
        private$reactives$tickers <- temp %>% arrange(Type, Name)
      }
    },
    
    subscribed = list(),
    
    # call callback functions registered earlier
    publish = function () {
      lapply(private$subscribed, function(f) f())
    }
  )
)