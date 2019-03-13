context("String manipulation")

test_that("Special symbols are converted to ASCII", {
  expect_identical(VHtools::as_ascii("ščřžýáí"), "scrzyai")
})