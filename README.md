
# geocard

<!-- badges: start -->
[![R build status](https://github.com/WorldHealthOrganization/geocard/workflows/R-CMD-check/badge.svg)](https://github.com/WorldHealthOrganization/geocard/actions)
<!-- badges: end -->

R package that creates an interactive visualization of the progression of case counts for a given geographic entity. Used in COVID-19 case counts displays.

## Installation

``` r
remotes::install_github("WorldHealthOrganization/geocard")
```

## Example

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
