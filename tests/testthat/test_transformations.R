context("Numeric vector transformations")

test_that("diff_fill() behaves as expected", {
  v1 <- rnorm(10)
  v2 <- VHtools::diff_fill(v1, lag = 1, differences = 1)
  v3 <- c(NA_real_, v1[-1] - v1[-10])
  expect_identical(v2, v3)
  
  v2 <- VHtools::diff_fill(v1, lag = 2, differences = 1)
  v3 <- c(NA_real_, NA_real_, diff(v1, lag = 2))
  expect_identical(v2, v3)
  
  v2 <- VHtools::diff_fill(v1, lag = 2, differences = 2)
  v3 <- c(rep(NA_real_, 4), diff(v1, lag = 2, differences = 2))
  expect_identical(v2, v3)
  
  v2 <- VHtools::diff_fill(v1, lag = length(v1), differences = 1)
  v3 <- rep(NA_real_, length(v1))
  expect_identical(v2, v3)
})

test_that("diff_basis() behaves as expected", {
  v1 <- rnorm(10)
  v2 <- VHtools::diff_basis(v1, t = 1:length(v1), tbase = 1, lag = 1)
  v3 <- c(v1[1], diff(v1))
  expect_identical(v2, v3)
})

