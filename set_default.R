## Rconfigure_default.R
library(utils)
## Using Internet Explorer proxy settings is
## often helpful in an IT controlled environment
#setInternet2(TRUE)
## Pre-select my nearest CRAN mirror in London
options(repos='http://cran.ma.imperial.ac.uk/')
## Set default directory for user installed packages
.libPaths(c("C:\\R_packages", .libPaths()))

