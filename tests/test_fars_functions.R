library(testthat)
library(dplyr)
library(essaipackage)

test_that("fars_read works properly", {
  # expect_that(fars_read("accident_2013.csv.bz2"), is_a("tibble"))
  expect_equal(nrow(fars_read("accident_2013.csv.bz2")), 30202L)
})
