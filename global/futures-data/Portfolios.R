
# collection of Portfolios

Portfolios <- R6Class("Portfolios",
                      
  public = list(
    
    initialize = function() {
      private$db = Db$new()$singleton  
    },
    
    load = function (nb = 3 ) {
      res <- dbSendQuery(private$db$portableDBI, "SELECT * FROM portfolios")
      portfoliosRaw <- dbFetch(res)
      dbClearResult(res)
      if (dim(portfoliosRaw)[1]>0) {
        private$list_p <- apply(portfoliosRaw, 1, function(row) {
          Portfolio$new( row["name"],
                         list(
                           assetsClasses = data.table(fromJSON(toString(row["assetClasses"]))),
                           assets = data.table(fromJSON(toString(row["assets"]))),
                           strategies = data.table(fromJSON(toString(row["strategies"])))))
        })
      } else {
        private$list_p <- lapply(1:3, function(x){
          Portfolio$new(paste0("Portfolio-", x))
        })
      }
#      isolate(self$backtestAll())
    },
    
    save = function() {
      res <- dbSendQuery(private$db$portableDBI, "DELETE FROM portfolios")
      dbClearResult(res)
      private$list_p %>% lapply(function(p) p$save())
      NULL
    },
    
    export = function(fname) {
      portfolios <- isolate(private$list_p) %>%
        lapply(function(p){
          data.table(name = p$name,
                     assetsClasses = toJSON(p$assetsClasses),
                     assets = toJSON(p$assets),
                     strategies = toJSON(p$strategies))
        }) %>% bind_rows()
      fwrite(portfolios, fname)
    },
    
    import = function(fname) {
      newportfolios <- fread(fname)
      if (dim(newportfolios)[1]>0) {
        private$list_p <- apply(newportfolios, 1, function(row) {
          print(paste("import", row["name"]))
          Portfolio$new( toString(row["name"]),
                         list(
                           assetsClasses = data.table(fromJSON(gsub("\"\"", "\"",row["assetsClasses"]))),
                           assets = data.table(fromJSON(gsub("\"\"", "\"",row["assets"]))),
                           strategies = data.table(fromJSON(gsub("\"\"", "\"",row["strategies"])))))
        })
      }
    },
    
    backtestAll = function() {
      lapply(private$list_p, function(p){
        p$backtest()
      })  
    },
    
    add = function(p) {
      p$selected <- TRUE
      private$list_p <- c(private$list_p, p)
      self$uniquify()
    },
    
    merge = function(name="Portfolio", mergeFrom=NULL) {
      if (!is.null(mergeFrom)) {
        sourcep <- private$list_p[unlist(child.up(private$list_p, "name")) %in% mergeFrom]
        strats <- child.up(sourcep, "strategies") %>% bind_rows() %>% data.table()
        assets <- child.up(sourcep, "assets") %>% 
          bind_rows() %>% 
          data.table() %>%
          filter(!is.na(Type)) %>%
          group_by(Code) %>%
          summarise(`Contract #` = min(`Contract #`),
                    Weight = min(Weight))
        assetsClasses <- child.up(sourcep, "assetsClasses") %>% 
          bind_rows() %>%
          data.table() %>% 
          group_by(Class) %>%
          summarise(Weight = sum(Weight))
        values <- list("assets"=assets, "strategies"=strats, "assetsClasses"=assetsClasses)
        newPortfolio <- Portfolio$new(name, values)
        self$add(newPortfolio)
      }
    },
    
    delete = function(name) {
      id <- match(name, unlist(child.up(private$list_p, "name")))[1]
      if (!is.na(id)) {
        private$list_p <- private$list_p[-id]
      }
    },
    
    uniquify = function() {
      uniques <- uniquify(unlist(child.up(private$list_p, "name")), sep="-")
      mapply(function(p, name){
        p$name <- name
      }, private$list_p, uniques)
    }
    
  ),
  
  active = list(
    
    # returns a list of portfolios
    list = function() {
      private$list_p
    },
    
    # returns a dataframe of computed backtest results
    series = function() {
      portfolios <- private$list_p %>% lapply(function (p){
        if (!is.null(p) && !is.null(p$show) && p$show) p
      }) %>%
        remove.nulls()
      
      temp<-data.frame()
      if (!is.null(portfolios) && length(portfolios)>0) {
        temp <- lapply(portfolios, function(p){
          p$series
        }) %>%
          bind_rows()
      }
      
      if (dim(temp)[1]>0) {
      tst<-  temp[,`:=`(period=min(period, na.rm=TRUE)),by="Date"] %>% 
          select(Date, period, series, Value) %>%
          spread(series, Value)
      } else {
        NULL
      }
    },
    
    positions = function() {
      portfolios <- private$list_p %>% lapply(function (p){
        if (!is.null(p) && !is.null(p$show) && p$show) p
      }) %>%
        remove.nulls()
      
      temp <- list()
      if (!is.null(portfolios) && length(portfolios)>0) {
        for (p in portfolios) {
          if (!is.null(p$positions))
            temp[[p$name]] <- p$positions
        }
      }
      if (length(temp)>0) {
        temp %>%
          bind_rows(.id = "Portfolio") %>%
          select(Portfolio, Date, period, Code, genericCode, Class,
                 completeCode, Position, Weight, multiple, rounded, 
                 netPosition, lev, Margin, maxDate)
      } else {
        data.frame(
          Portfolio = character(), 
          Date = as.Date(numeric()),
          period = numeric(),
          Code = character(),
          genericCode = character(), 
          Class = character(),
          completeCode = character(), 
          Position = numeric(), 
          Weight = numeric(),
          multiple = numeric(),
          rounded = numeric(),
          netPosition = numeric(),
          lev = numeric(),
          Margin = numeric(),
          maxDate = as.Date(numeric()))
      }
    }
  ),
  
  private = list(
    db = NULL,
    list_p=list()
  )
  
)