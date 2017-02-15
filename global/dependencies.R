
dependencies <- c(
  "data.table",
  "digest",
  "dplyr",
  "dtplyr",
  "dygraphs",
  "ggplot2",
  "jsonlite",
  "lubridate",
  "magrittr",
  "PerformanceAnalytics",
  "R6",
  "rhandsontable",
  "RCurl",
  "RSQLite",
  "scales",
  "shiny",
  "shinythemes",
  "tidyr",
  "XML",
  "xts"
  )

new.packages <- dependencies[!(dependencies %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages)

lapply(dependencies, library, character.only = TRUE)

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

rm(dependencies, new.packages)