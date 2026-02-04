test_that("concordance calculates index correctly", {
  set.seed(123)
  x <- cumsum(rnorm(100))
  y <- x + rnorm(100, sd = 0.5)
  
  result <- concordance(x, y)
  
  expect_type(result, "list")
  expect_s3_class(result, "concordance")
  expect_named(result, c("concordance", "expected", "adjusted", "n", "method", "p_positive"))
  
  # Check value ranges
  expect_gte(result$concordance, 0)
  expect_lte(result$concordance, 1)
  expect_gte(result$adjusted, -1)
  expect_lte(result$adjusted, 1)
})

test_that("concordance for perfect co-movement", {
  x <- 1:50
  y <- 2:51  # Perfectly follows x
  
  result <- concordance(x, y, method = "harding-pagan")
  
  expect_equal(result$concordance, 1)
  # Adjusted might be NA if expected = 1
  if (!is.na(result$adjusted)) {
    expect_gte(result$adjusted, 0.5)
  }
})

test_that("concordance for independent series", {
  set.seed(456)
  x <- cumsum(rnorm(100))
  y <- cumsum(rnorm(100))
  
  result <- concordance(x, y)
  
  # For independent series, concordance should be near 0.5
  expect_gt(result$concordance, 0.3)
  expect_lt(result$concordance, 0.7)
  
  # Adjusted should be near 0
  expect_gt(result$adjusted, -0.3)
  expect_lt(result$adjusted, 0.3)
})

test_that("concordance for counter-cyclical series", {
  set.seed(789)
  x <- cumsum(rnorm(100))
  y <- -x + rnorm(100, sd = 0.5)
  
  result <- concordance(x, y)
  
  # Should have low concordance
  expect_lt(result$concordance, 0.3)
  
  # Adjusted should be negative
  expect_lt(result$adjusted, 0)
})

test_that("concordance handles equal length requirement", {
  x <- 1:10
  y <- 1:9
  
  expect_error(
    concordance(x, y),
    "same length"
  )
})

test_that("concordance simple method works", {
  x <- c(1, 3, 2, 5, 4)
  y <- c(2, 4, 3, 6, 5)
  
  result <- concordance(x, y, method = "simple")
  
  expect_equal(result$method, "simple")
  expect_type(result$concordance, "double")
})

test_that("concordance handles NA values", {
  x <- c(1, 3, NA, 5, 4, 6)
  y <- c(2, 4, 3, 6, 5, 7)
  
  result <- concordance(x, y)
  
  # Should handle NAs gracefully
  expect_type(result$concordance, "double")
  expect_lt(result$n, 6)  # Some observations dropped
})

test_that("concordance print method exists", {
  x <- 1:20
  y <- 2:21
  
  result <- concordance(x, y)
  
  # Test that print doesn't error
  expect_output(print(result))
})
