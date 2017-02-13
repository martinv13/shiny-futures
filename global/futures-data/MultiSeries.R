
# multiple series to display

MultiSeries <- R6Class("MultiSeries",
  
  public = list(
    
    initialize = function (data) {
      private$data <- data
      
      concat <- data %>%
        bind_rows(.id="series")
      

      private$series_p <- concat %>%
        select(series, Date, period, Value) %>%
        spread(series, Value)
      
      private$rollpoints_p <- concat %>%
        group_by(series) %>%
        arrange(Date) %>%
        filter(completeCode != lag(completeCode) | Position != lag(Position)) %>%
        transmute(series = series,
                  Date = Date,
                  Text = completeCode,
                  Position = Position)
      
    }

  ),
  
  active = list(
    
    series = function() {
      private$series_p
    },
    
    rollpoints = function() {
      private$rollpoints_p
    }
  ),
  
  private = list(
    data = NULL,
    series_p = NULL,
    rollpoints_p = NULL
  )
  
)