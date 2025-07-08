
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rfits

<!-- badges: start -->

![R-CMD-check](https://github.com/asgr/Rfits/workflows/R-CMD-check/badge.svg)
<!-- badges: end -->

The goal of Rfits is to create a mid to high level interface to
astronomy FITS files.

## Installation

You can obtain the current version from [GitHub](https://github.com/)
with:

``` r
# install.packages("remotes")
remotes::install_github("asgr/Rfits")
```

Note, due to CFITSIO itself being written with very old C standards (90s) some users might have install issues unless they explicitly set the C standard to use. Luckily in R this is fairly easy. If you don't already have a file at ~/.R/Makevars create one (just an empty text file), or if one is already there edit or create the relevant line:

```
CC = clang -std=gnu17
```

if using Clang, or

```
CC = gcc -std=gnu17
```

We have lodged an [Issue](https://github.com/HEASARC/cfitsio/issues/62) with the CFITSIO folk, so hopefully this workaround will not be necessary for too long!

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(Rfits)
## basic example code
file_image=system.file('extdata', 'image.fits', package = "Rfits")
temp_image=Rfits_read_image(file_image, header=TRUE)
file_table = system.file('extdata', 'table.fits', package = "Rfits")
temp_table = Rfits_read_table(file_table, header=TRUE)

data=list(temp_image, temp_table)
  
file_mix_temp = tempfile()

Rfits_write_all(data, file_mix_temp)

data2 = Rfits_read_all(file_mix_temp)

sum(data[[1]]$imDat - data2[[1]]$imDat)
#> [1] 0

cols_check = which(sapply(temp_table[1,], is.numeric))
sum(data[[2]][,..cols_check] - data2[[2]][,..cols_check])
#> [1] 0
```
