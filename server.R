## App server

shinyServer(function(input, output, session) {
  
  callModule(updateTab, "updateTab")
  callModule(overviewTab, "overviewTab")
  callModule(exploreTab, "exploreTab")
  callModule(backtestTab, "backtestTab")

<<<<<<< HEAD
=======
    
>>>>>>> e446b9e8ef27f0408916be2c11693fd9caee5db5
})