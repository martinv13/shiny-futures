# Shiny-Futures
A Shiny app to work with futures contracts data.
This project is under development.

The (soft) roadmap is to enable user:
 - fetching individual futures contracts data from Quandl;
 - compute and display roll strategies, analyse distributions, term structures and correlations;
 - backtest trading strategies and strategies portfolios;
 
Computing is mostly based on data.table / dplyr. Dygraphs is used for rendering. A Sqlite DB holds the data.

### Update data
This tab lists available contracts and manage downloads from Quandl.

[![Update tab](/screenshots/update-s.png)](/screenshots/update.png)

### Overview
This tab display main contracts features (volatility, correlation,...).

[![Overview tab](/screenshots/overview-s.png)](/screenshots/overview.png)

### Explore
This tab allows displaying individual contracts and strategies, with custom roll rules.

[![Explore tab](/screenshots/explore-s.png)](/screenshots/explore.png)

### Backtest
This tab allows backtesting and comparing multiple portfolios of contracts and strategies.

[![Backtest tab](/screenshots/backtest-s.png)](/screenshots/backtest.png)

### Give it a try

``` R
library(shiny)
runGitHub("shiny-futures", "martinv13")
```
