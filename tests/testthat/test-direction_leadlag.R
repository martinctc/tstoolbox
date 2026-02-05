test_that("direction_leadlag basic functionality works", {
  set.seed(123)
  x <- cumsum(rnorm(100))
  y <- cumsum(rnorm(100))
  
  result <- direction_leadlag(x, y, max_lag = 5)
  
  expect_type(result, "list")
  expect_s3_class(result, "direction_leadlag")
  expect_named(result, c("optimal_lag", "max_comovement", "lag_table", "interpretation"))
  
  # Check lag_table structure
  expect_s3_class(result$lag_table, "data.frame")
  expect_equal(nrow(result$lag_table), 11)  # -5 to +5
  expect_true("lag" %in% names(result$lag_table))
  expect_true("comovement" %in% names(result$lag_table))
  
  # Check value ranges
  expect_gte(result$max_comovement, 0)
  expect_lte(result$max_comovement, 1)
  expect_gte(result$optimal_lag, -5)
  expect_lte(result$optimal_lag, 5)
})

test_that("direction_leadlag detects leading relationship", {
  # x leads y by 2 periods
  set.seed(456)
  x <- cumsum(rnorm(100))
  y <- dplyr::lag(x, 2) + rnorm(100, sd = 0.3)
  
  result <- direction_leadlag(x, y, max_lag = 5)
  
  # Should detect x leads (negative lag)
  expect_lte(result$optimal_lag, 0)
  expect_gt(result$max_comovement, 0.6)
})

test_that("direction_leadlag detects lagging relationship", {
  # y leads x by 3 periods (x lags y)
  set.seed(789)
  y <- cumsum(rnorm(100))
  x <- dplyr::lag(y, 3) + rnorm(100, sd = 0.3)
  
  result <- direction_leadlag(x, y, max_lag = 5)
  
  # Should detect positive lag (y leads)
  expect_gte(result$optimal_lag, 0)
  expect_gt(result$max_comovement, 0.6)
})

test_that("direction_leadlag detects contemporaneous relationship", {
  # No lag
  set.seed(999)
  x <- cumsum(rnorm(100))
  y <- x + rnorm(100, sd = 0.5)
  
  result <- direction_leadlag(x, y, max_lag = 3)
  
  # Optimal lag should be 0 or very close
  expect_lte(abs(result$optimal_lag), 1)
})

test_that("direction_leadlag validates input", {
  x <- 1:10
  y <- 1:5
  
  expect_error(direction_leadlag(x, y), 
               "`x` and `y` must have the same length")
  
  x <- 1:10
  y <- 1:10
  expect_error(direction_leadlag(x, y, max_lag = 0), 
               "`max_lag` must be at least 1")
})

test_that("direction_leadlag handles different max_lag values", {
  set.seed(111)
  x <- cumsum(rnorm(100))
  y <- cumsum(rnorm(100))
  
  result_3 <- direction_leadlag(x, y, max_lag = 3)
  result_10 <- direction_leadlag(x, y, max_lag = 10)
  
  expect_equal(nrow(result_3$lag_table), 7)   # -3 to +3
  expect_equal(nrow(result_10$lag_table), 21) # -10 to +10
})

test_that("direction_leadlag interpretation is informative", {
  set.seed(222)
  x <- cumsum(rnorm(80))
  y <- cumsum(rnorm(80))
  
  result <- direction_leadlag(x, y)
  
  expect_type(result$interpretation, "character")
  expect_gt(nchar(result$interpretation), 20)
  expect_true(grepl("co-movement|lead|lag|contemporaneous", 
                    result$interpretation, ignore.case = TRUE))
})

test_that("direction_leadlag handles edge cases", {
  # Very short series
  x <- c(1, 2, 3, 4, 5)
  y <- c(2, 3, 4, 5, 6)
  
  result <- direction_leadlag(x, y, max_lag = 1)
  expect_s3_class(result, "direction_leadlag")
  
  # Series with ties
  x <- c(1, 1, 2, 2, 3, 3, 4, 4)
  y <- c(1, 2, 2, 3, 3, 4, 4, 5)
  
  expect_no_error(direction_leadlag(x, y, max_lag = 2))
})
