
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
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag

# Prep example data
minicars <- head(mtcars)

# Select columns
select(minicars, c("mpg", "hp"))
#>                    mpg  hp
#> Mazda RX4         21.0 110
#> Mazda RX4 Wag     21.0 110
#> Datsun 710        22.8  93
#> Hornet 4 Drive    21.4 110
#> Hornet Sportabout 18.7 175
#> Valiant           18.1 105

# Rename Columns
rename(minicars, c("miles_per_gallon" = "mpg", "horsepower" = "hp"))
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
count(minicars, "cyl")
#>   cyl n
#> 1   4 1
#> 2   6 4
#> 3   8 1

# Filter data.frame using a function
filter(minicars, function(d) { d$mpg > 20 & d$cyl == 6 })
#>                 mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4      21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Hornet 4 Drive 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1

# Filter data.frame based on a logical vector
keep_rows(minicars, minicars$mpg > 20 & minicars$cyl == 6)
#>                 mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4      21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Hornet 4 Drive 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1

# Count unique levels in a vector
n_distinct(mtcars)
#> [1] 32

# Lag and lead
lag(1:5)
#> [1] NA  1  2  3  4
lead(1:5)
#> [1]  2  3  4  5 NA
```

## For Developers

Ensure PRs are formatted with
[air](https://posit-dev.github.io/air/formatter.html).

## Deciding when to use dfx

**Performing a data analysis for research / datavis:**

- Consider using the tidyverse/data.table instead

**Developing a package with only base R dependencies:**

- data.table is blazingly fast, stable, and if installing from CRAN all
  you need is R. I highly recommend it for package dev. It also includes
  c code so if you have to compile from source you may need additional
  packages.

**Developing a package from a tidyverse style analysis:**

- Theres nothing wrong with using dplyr and other tidyverse tools in
  packages. The tidyverse team is great at lifecycle management, so if
  you depend on their tools you’ll start seeing warnings and migration
  instructions long before any deprecated functions / approaches get
  removed. However, the APIs have historically been improved over time,
  changing much more than datatable, so expect to have to update your
  packages every now and then. If you want your package to be set and
  forget, data.table is a great option but you lose some of the great
  tidyverse API design & porting can be a bit of work. dfx seeks to sit
  between these. Expressive, functional design inspired by the
  tidyverse, but without support for the non-standard evaluation that
  IMHO makes interactive analysis speedy to write but package codebases
  more difficult to reason about.

dfx does not use any C, C++ or rust to speed up operations. This makes
it substantially slower than data.table, but unbelievably easy to
install from source on any platform.
