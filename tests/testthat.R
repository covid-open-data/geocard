library(testthat)
library(geocard)

test_check("geocard")

test_that("geocard runs", {
  a <- geocard(wa_cases, "Washington", min_date = as.Date("2020-03-01"))
  expect_true(inherits(a, "htmlwidget"))
})
