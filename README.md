
# CoRviTools

<!-- badges: start -->
<!-- badges: end -->

The goal of CoRviTools is to provide some useful functions.

## Installation

You can install the development version of CoRviTools like so:

``` r
if(!require(devtools)) {
  install.packages("devtools")
  require(devtools)}
devtools::install_github("dcorvi/corvitools")
```


``` r
library(CoRviTools)
# Map a drive
set_mapped_drive(d = file.path("noaa.gov","home38", "dcorvi") , user_name = "dcorvi", password = key_get("woods hole network", "dcorvi"))

Use the vigettes in this package to walk through the process of running this function:
vignette(package="corvitools")

```

