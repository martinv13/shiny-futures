# Shiny-Futures
A Shiny app to work with futures contracts data.
This project is under development.

The (soft) roadmap is to enable user:
 - fetching individual futures contracts data from Quandl;
 - compute and display roll strategies, analyse distributions, term structures and correlations;
 - backtest trading strategies and strategies portfolios;
 
Computing is mostly based on data.table / dplyr. Dygraphs is used for rendering. A Sqlite DB holds the data.
