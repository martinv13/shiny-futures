
Singleton <- R6Class("Singleton",
  
  private = list(
    senv=new.env()
  ),
  
  active = list(
    singleton = function() {
      if (is.null(private$senv$sgt)) {
        private$senv$sgt <- Singleton$new()
      }
      private$senv$sgt
  })
  
)