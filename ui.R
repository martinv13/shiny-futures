
## App UI

navbarPage("Futures",theme = shinytheme("flatly"),
  updateTabUI("updateTab", "Update"),
  overviewTabUI("overviewTab", "Overview"),
  exploreTabUI("exploreTab", "Explore"),
  backtestTabUI("backtestTab", "Backtest"))