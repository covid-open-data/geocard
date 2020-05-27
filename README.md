
# geocard

<!-- badges: start -->
[![R build status](https://github.com/covid-open-data/geocard/workflows/R-CMD-check/badge.svg)](https://github.com/covid-open-data/geocard/actions)
<!-- badges: end -->

The goal of geocard is to ...

## Installation

``` r
remotes::install_github("covid-open-data/geocard")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(geocard)

geocard( 
  wa_cases, 
  card_name = "Washington", 
  population = 7549403, 
  ref_source = "NYT", 
  img_url = "https://raw.githubusercontent.com/hafen/us-locator-maps/master/thumbs/admin1/US/53.png" 
)
```
