## gophr <img src='man/figures/logo.png' align="right" height="120" />

MER Structured Dataset utilities package

<!-- badges: start -->
[![R-CMD-check](https://github.com/USAID-OHA-SI/gophr/workflows/R-CMD-check/badge.svg)](https://github.com/USAID-OHA-SI/gophr/actions)
[![gophr status badge](https://usaid-oha-si.r-universe.dev/badges/gophr)](https://usaid-oha-si.r-universe.dev/gophr)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![:name status badge](https://usaid-oha-si.r-universe.dev/badges/:name)](https://usaid-oha-si.r-universe.dev/)
<!-- badges: end -->


## Overview

The function herewithin are a set of utility functions related to working with the MER Structured Dataset and are complimented by other USAID-OHA-SI packages such as `glamr`, `glitr` and `gisr`. Focal users are analysts in USAID/GH/OHA who are using R to pull data from DATIM or perform the same repeated functions each quarter like creating TX_NET_NEW targets or assessing achievement.


## Installation

`gophr` is not on CRAN, so you will have to install it directly from [rOpenSci](https://usaid-oha-si.r-universe.dev/packages) or [GitHub](https://github.com/USAID-OHA-SI/) using the code found below.

``` r
## SETUP

  #install from rOpenSci
    install.packages('gophr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
    
  #alt: install from GitHub using pak
    #install.packages("pak")
    #pak::pak("USAID-OHA-SI/gagglr")
    
  #load the package
    library(gophr)

## LIST OF FUNCTIONS INCLUDED WITH PACKAGE
  ls(package:gophr)
```


---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*
