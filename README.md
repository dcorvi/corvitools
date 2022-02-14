
# corvitools

<!-- badges: start -->
<!-- badges: end -->

The goal of corvitools is to provide some useful functions.

## Installation

You can install the development version of corvitools like so:

``` r
if(!require(devtools)) {
  install.packages("devtools")
  require(devtools)}
devtools::install_github("dcorvi/corvitools")

install.packages("keyring")
install.packages("stringr")
install.packages("parallel")
library(keyring)
library(stringr)
library(parallel)

```


``` r
library(corvitools)
# Map a drive
set_mapped_drive(d = file.path("noaa.gov","home38", "dcorvi") , user_name = "dcorvi", password = key_get("woods hole network", "dcorvi"))

Use the vigettes in this package to walk through the process of running this function:
vignette(package="corvitools")

```

