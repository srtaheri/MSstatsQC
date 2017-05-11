library(testthat)
library(MSstatsQC)

test_check("MSstatsQC")

test_that("ChangePointEstimator is working well", {
  expect_equal(str_length("a"), 1)
  expect_equal(str_length("ab"), 2)
  expect_equal(str_length("abc"), 3)
})
