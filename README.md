
<!-- README.md is generated from README.Rmd. Please edit that file -->

# libminer

<!-- badges: start -->

[![R-CMD-check](https://github.com/ateucher/libminer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ateucher/libminer/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of libminer is to provide an overview of your R library setup.
It is a toy package created as part of a workshop and not meant for
serious use!

## Installation

You can install the development version of libminer from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ateucher/libminer")
```

## Example

To get a count of installed packages in each of your libraries,
optionally with the total sizes, use `lib_summary()`.

``` r
library(libminer)

lib_summary()
#>                                                                                        Library
#> 1                         /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/library
#> 2 /private/var/folders/_f/n9fw7ctx3fqf2ty9ylw502g80000gn/T/RtmpGENjkD/temp_libpathc5b067a9b760
#> 3                                                      /Users/andy/Library/R/arm64/4.3/library
#>   n_packages
#> 1         29
#> 2          1
#> 3        263
# specify sizes = TRUE
lib_summary(sizes = TRUE)
#>                                                                                        Library
#> 1                         /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/library
#> 2 /private/var/folders/_f/n9fw7ctx3fqf2ty9ylw502g80000gn/T/RtmpGENjkD/temp_libpathc5b067a9b760
#> 3                                                      /Users/andy/Library/R/arm64/4.3/library
#>   n_packages   lib_size
#> 1         29   71351828
#> 2          1      14596
#> 3        263 1623464118
```
