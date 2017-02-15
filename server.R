## App server

shinyServer(function(input, output, session) {
  
  callModule(updateTab, "updateTab")
  callModule(overviewTab, "overviewTab")
  callModule(exploreTab, "exploreTab")
  callModule(backtestTab, "backtestTab")

    
})