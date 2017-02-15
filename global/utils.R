
# remove null elements in a list 
remove.nulls <- function(l){
  if (is.list(l)) {
    rem <- (1:length(l))[unlist(lapply(l, is.null))]
    if (length(rem)>0){
      l[-rem]     
    } else {
      l
    }
  } else if (is.vector(l)) {
    l[!is.null(l)]
  } else {
    l
  }
}

# move up properties accessed by "$" in a list of objects
child.up <- function(l, k){
  if (is.list(l)) {
    sapply(l, function(x) {
      x[[k]]
    }, simplify = FALSE, USE.NAMES = TRUE)    
  } else {
    l
  }
}

uniquify <- function(x, sep=".") ave(x, x, FUN = uniquify_2(sep))
uniquify_2 <- function(sep="."){
  function(x) {
    if (length(x)==1) x else {
      paste0(x, c("", paste0(sep,1:(length(x)-1))))
    }
  }
}
 

debounce <- function(expr, millis, env = parent.frame(), quoted = FALSE,
                     domain = getDefaultReactiveDomain()) {
  
  force(millis)
  
  f <- exprToFunction(expr, env, quoted)
  label <- sprintf("debounce(%s)", paste(deparse(body(f)), collapse = "\n"))
  
  v <- reactiveValues(
    trigger = NULL,
    when = NULL # the deadline for the timer to fire; NULL if not scheduled
  )  
  
  # Responsible for tracking when f() changes.
  observeEvent(f(), {
    # The value changed. Start or reset the timer.
    v$when <- Sys.time() + millis/1000
  }, ignoreNULL = FALSE)
  
  # This observer is the timer. It rests until v$when elapses, then touches
  # v$trigger.
  observe({
    if (is.null(v$when))
      return()
    
    now <- Sys.time()
    if (now >= v$when) {
      v$trigger <- runif(1)
      v$when <- NULL
    } else {
      invalidateLater((v$when - now) * 1000, domain)
    }
  })
  
  # This is the actual reactive that is returned to the user. It returns the
  # value of f(), but only invalidates/updates when v$trigger is touched.
  eventReactive(v$trigger, {
    f()
  }, ignoreNULL = FALSE)
}


# in data.frame "dt", lookup ... columns and copy the first not null into "to"
notNull <-  function(dt, to, ...){
  from <- unlist(list(...))
  cn <- colnames(dt)
  eq <- match(cn, from)
  nn <- match(FALSE, is.na(eq))
  if(is.na(nn)){
    dt[[to]] <- rep(NA, dim(dt)[1])
  } else {
    dt[[to]] <- dt[[cn[nn]]]
  }
  dt
}

ifNull <- function (test, repl) {
  if (is.null(test)) repl else test
}
ifNotNumeric <- function (test, repl) {
  test <- as.numeric(test)
  if (is.null(test) || length(test)==0 || is.na(test)) repl else test
}
ifNA <- function (test, repl) {
  if (is.na(test)) repl else test
}






