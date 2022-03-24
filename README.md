
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rforceplots

<!-- badges: start -->
<!-- badges: end -->

rforceplots is as a stand-alone wrapper for the
[shapjs](https://www.npmjs.com/package/shapjs) node package to
facilitate rendering additive force plots in R. It does not require
Python or reticulate and is compatible with objects exported by the
`shapper::individual_variable_effect()` and `shapr::explain()`
functions. It’s compatible with Shiny and gives access to the three base
visualization functions from the shapjs node package for custom SHAP
explanation formats.

## Installation

You can install the development version of rforceplots like so:

``` r
remotes::install_github("rhenkin/rforceplots")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rforceplots)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.
