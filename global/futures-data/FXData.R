
# data source

FXData <- R6Class("FXData",
                      
  cloneable = FALSE,
  
  public = list(
    
    # initialize DataSource with a db connection object
    initialize = function () {
      private$db <- Db$new()$singleton
    },
    
    # load data from db
    load = function() {
      
      private$fetchCodes()
      
      selectedCodes <- self$codes$shortName
      
      # extract data from db
      rates_data <- tbl(private$db$localDPL, "fx_data")
      if (length(selectedCodes) == 0) {
        rates_data <- data.table(shortName=character())
      } else {
        if (length(selectedCodes) == 1) {
          rates_data %<>% filter(shortName == selectedCodes)
        } else {
          rates_data %<>% filter(shortName %in% selectedCodes)
        }
        rates_data %<>%
          collect() %>%
          data.table()
      } 
      if (length(rates_data$shortName)==0) {
        return (FALSE)
      }

      extr.data <- rates_data$data %>% lapply(function(x) {
        unserialize(x) %>% data.table()
      })
      names(extr.data) <- rates_data$shortName
      
      self$data <- extr.data %>% bind_rows(.id = "shortName") 
      
      NULL
    },
    

    
    
    # store contracts data
    data = NULL
  ),
  
  active = list(    
    
    # returns a reactive data.frame 
    codesReactive = function () {
      reactive({
        private$reactives$codes
      })
    },
    
    # returns tickers data
    codes = function (ncodes = NULL) {
      if (!is.null(ncodes)) {
        ncodes %<>% select(shortName, Name, `Quandl Code`, codeFrom, codeTo)
        if(length(ncodes$Name)>0) {
          quer <- paste0("DELETE FROM fx WHERE shortName NOT IN (",
                         paste(paste0("\"", ncodes$shortName, "\""), collapse = ","),
                         ")")
          res <- dbSendQuery(private$db$portableDBI, quer)
          dbClearResult(res)
        }
        for (i in 1:length(ncodes$shortName)) {
          if (!is.na(ncodes$shortName[i])) {
            quer <- paste0("INSERT OR REPLACE INTO fx(",
                           paste0(paste0("\"", names(ncodes), "\""), collapse = ","),
                           ") VALUES(",
                           paste0(ifelse(sapply(ncodes[1,],is.numeric), ncodes[i,], paste0("\"", ncodes[i,], "\"")), collapse = ","),
                           ")")
          } else {
            quer <- paste0("INSERT OR REPLACE INTO fx(shortName, Name, \"Quandl Code\",",
                           "codeFrom, codeTo) values(\"new\",\"new\", \"new\", \"EUR\", \"USD\")")
          }
          res <- dbSendQuery(private$db$portableDBI, quer)
          dbClearResult(res)
        }
        private$fetchCodes()
      }
      data.table(isolate(private$reactives$codes))
    },
    
    singleton = function() {
      if (is.null(private$senv$sgt)) {
        private$senv$sgt <- FXData$new()
      }
      private$senv$sgt
    }
  
  ), 
  
  private = list(
    db = NULL,
    senv = new.env(),
    
    reactives = reactiveValues(codes = NULL),
    
    # fetch codes from DB; called after update
    fetchCodes = function() {
      codes_db <- tbl(private$db$portableDPL, "fx")
      rates_db <- tbl(private$db$localDPL, "fx_data")
      if (!is.null(codes_db) && !is.null(rates_db)) {
        fetch_data <- rates_db %>% 
          select(shortName, lastFetch) %>%
          collect() %>% data.table()
        private$reactives$codes <- codes_db %>% 
          collect() %>% 
          data.table() %>%
          left_join(fetch_data, by="shortName")
      }
    }
    
  )
)