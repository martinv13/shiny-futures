
CoTData <- R6Class("CoTData",

  public = list(
    
    uris = list(
      financials = list(uri="http://www.cftc.gov/files/dea/history/fut_fin_txt_2017.zip",
                        fname="FinFutYY.txt")
    ),
    
    fetch = function() {
      
      f <- self$uris[[1]]

      tempf <- tempfile()
      download.file(f$uri, tempf, quiet=TRUE)
      unzip(tempf,  exdir = "local")
      unlink(tempf)
      cot.data <- fread(paste0("local/", f$fname))
      
      cot.data[, Lev_pos := Lev_Money_Positions_Long_All/
                 (Lev_Money_Positions_Long_All+Lev_Money_Positions_Short_All)*2-1]
      
      last.report <- cot.data[
        cot.data[order(`Report_Date_as_YYYY-MM-DD`, decreasing = TRUE),
                 .I[1],
                 by=Market_and_Exchange_Names]$V1
      ]
      
      ggplot(cot.data[order(Market_and_Exchange_Names,
                            `Report_Date_as_YYYY-MM-DD`)], aes(x = Open_Interest_All, 
                           y = Lev_pos, 
                           group = Market_and_Exchange_Names,
                           color = Market_and_Exchange_Names)) +
        geom_path() + 
        geom_point(data=last.report, 
                   aes(x = Open_Interest_All, 
                       y = Lev_pos, 
                       group = Market_and_Exchange_Names,
                       color = Market_and_Exchange_Names)) +
        theme(legend.position = "bottom") +
        scale_x_log10()
      
      
      cot.data[`Report_Date_as_YYYY-MM-DD` == max(`Report_Date_as_YYYY-MM-DD`),
               Lev_pos,
               by=Market_and_Exchange_Names][order(Lev_pos),]

    
    }
    
  )
  
)