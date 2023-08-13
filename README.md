
<!-- README.md is generated from README.Rmd. Please edit that file -->

# differential

<!-- badges: start -->
<!-- badges: end -->

The goal of `differential` is to provide Automatic Differentiation for
arbitrary R code.

## Installation

You can install the development version of differential from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pkg_install("fangzhou-xie/differential")
```

## Example

### Automatic differentiation

``` r
library(differential)

e <- quote(3*x^2)
gradient(e, wrt(e))
#> 6 * x
```

### (Simple) Algebraic Simplification

``` r
e <- quote(3*x^2+a*x^2)
simplify(e)
#> x^2 * (3 + a)
```
