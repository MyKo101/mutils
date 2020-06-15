
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mutils <img src='man/figures/logo.png' align="right" height="138.5" />

<!-- badges: start -->

[![R build
status](https://github.com/MyKo101/mutils/workflows/R-CMD-check/badge.svg)](https://github.com/MyKo101/mutils/actions)
[![Version
Badge](https://img.shields.io/badge/Version-0.0.0.9013-orange.svg)](https://github.com/MyKo101/mutils)

<!-- badges: end -->

The goal of mutils is to provide useful functions to make data
processing smoother. Most functions contained here are “nifty”, rather
than “innovative”.

## Installation

The development version from [GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("MyKo101/mutils")

## Overview

Overall, this package is just a collection of functions that I’ve
created as a data scientist. It is far from exhaustive, but it contains
many of the functions that I find myself using repeatedly. Some are
simple formula, some are designed to make processes quicker and some
create tidier outputs (to my personal standards). There may be
simpler/faster/more obvious ways to do many of the things contained in
here, and improvements on speed/efficiency are more than welcome.

## Code of Conduct

Please note that the mutils project is released with a [Contributor Code
of Conduct](https://michaelbarrowman.co.uk/mutils/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Testing tibbles

``` r
load_packages("tibble")
#> Loading required namespace: purrr
tibble(x=runif(10),y=runif(10))
#> [90m# A tibble: 10 x 2[39m
#>         x      y
#>     [3m[90m<dbl>[39m[23m  [3m[90m<dbl>[39m[23m
#> [90m 1[39m 0.576  0.282 
#> [90m 2[39m 0.336  0.695 
#> [90m 3[39m 0.441  0.049[4m3[24m
#> [90m 4[39m 0.386  0.388 
#> [90m 5[39m 0.644  0.744 
#> [90m 6[39m 0.022[4m3[24m 0.290 
#> [90m 7[39m 0.594  0.554 
#> [90m 8[39m 0.024[4m7[24m 0.809 
#> [90m 9[39m 0.175  0.890 
#> [90m10[39m 0.476  0.737
```
