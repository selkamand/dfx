
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dfx

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://www.r-pkg.org/badges/version/dfx)](https://cran.r-project.org/package=dfx)
[![R-CMD-check](https://github.com/selkamand/dfx/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/selkamand/dfx/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/selkamand/dfx/graph/badge.svg)](https://app.codecov.io/gh/selkamand/dfx)
![GitHub Issues or Pull
Requests](https://img.shields.io/github/issues-closed/selkamand/dfx)
[![code
size](https://img.shields.io/github/languages/code-size/selkamand/dfx.svg)](https://github.com/selkamand/dfx)
![GitHub last
commit](https://img.shields.io/github/last-commit/selkamand/dfx)
<!-- badges: end -->

dfx is a data.frame transformation package with a tidy-like interface
but written in base-R without non-standard evaluation. Designed as a
lightweight and stable dependency for R package development.

## Installation

You can install the development version of dfx like so:

``` r
# install.packages('remotes')
remotes::install_github("selkamand/dfx")
```

## Quick Start

``` r
library(dfx)
#> 
#> Attaching package: 'dfx'
#> The following object is masked from 'package:stats':
#> 
#>     lag

# Select columns
head(select(mtcars, c("mpg", "hp")))
#>                    mpg  hp
#> Mazda RX4         21.0 110
#> Mazda RX4 Wag     21.0 110
#> Datsun 710        22.8  93
#> Hornet 4 Drive    21.4 110
#> Hornet Sportabout 18.7 175
#> Valiant           18.1 105

# Rename Columns
head(rename(mtcars, c("miles_per_gallon" = "mpg", "horsepower" = "hp")))
#>                   miles_per_gallon cyl disp horsepower drat    wt  qsec vs am
#> Mazda RX4                     21.0   6  160        110 3.90 2.620 16.46  0  1
#> Mazda RX4 Wag                 21.0   6  160        110 3.90 2.875 17.02  0  1
#> Datsun 710                    22.8   4  108         93 3.85 2.320 18.61  1  1
#> Hornet 4 Drive                21.4   6  258        110 3.08 3.215 19.44  1  0
#> Hornet Sportabout             18.7   8  360        175 3.15 3.440 17.02  0  0
#> Valiant                       18.1   6  225        105 2.76 3.460 20.22  1  0
#>                   gear carb
#> Mazda RX4            4    4
#> Mazda RX4 Wag        4    4
#> Datsun 710           4    1
#> Hornet 4 Drive       3    1
#> Hornet Sportabout    3    2
#> Valiant              3    1


# Count observations in data.frame column
count(mtcars, "cyl")
#>   cyl  n
#> 1   4 11
#> 2   6  7
#> 3   8 14

# Count unique levels in a vector
n_distinct(mtcars)
#> [1] 32

# Lag and lead
lag(1:5)
#> [1] NA  1  2  3  4
lead(1:5)
#> [1]  2  3  4  5 NA
```
