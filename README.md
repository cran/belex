
<!-- README.md is generated from README.Rmd. Please edit that file -->
belex
=====

belex downloads historical financial time series from the Belgrade Stock Exchange. One can specify which ticker or index to download, start and end date.

Installation
------------

Install the release version from CRAN:

``` r
install.packages("belex")
```

Example
-------

``` r
library(belex)

blx15 <- belex("belex15")

niis <- belex("NIIS", "2011-01-01", "2015-12-31")
```
