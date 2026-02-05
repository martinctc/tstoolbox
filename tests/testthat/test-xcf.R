test_that("xcf creates cross-correlation table", {
  set.seed(123)
  n <- 50
  df <- data.frame(
    x = cumsum(rnorm(n)),
    y = cumsum(rnorm(n))
  )
  
  result <- xcf(df, x, y)
  
  expect_s3_class(result, "data.frame")
  expect_true("lag" %in% names(result))
  expect_true("x.corr" %in% names(result))
  
  # Should have both positive and negative lags
  expect_true(any(result$lag > 0))
  expect_true(any(result$lag < 0))
  expect_true(any(result$lag == 0))
})

test_that("xcf returns cross-correlation results", {
  df <- data.frame(
    x = 1:30,
    y = 2:31
  )
  
  result <- xcf(df, x, y)
  
  # Should have rows with lags
  expect_gt(nrow(result), 0)
  expect_true(max(result$lag) > 0)
  expect_true(min(result$lag) < 0)
})

test_that("xcf correlation values are in valid range", {
  set.seed(456)
  df <- data.frame(
    x = rnorm(50),
    y = rnorm(50)
  )
  
  result <- xcf(df, x, y)
  
  expect_true(all(result$x.corr >= -1))
  expect_true(all(result$x.corr <= 1))
})

test_that("xcf with perfectly correlated series", {
  x <- 1:20
  df <- data.frame(x = x, y = x)
  
  result <- xcf(df, x, y)
  
  # Lag 0 should have correlation 1
  lag_0 <- result$x.corr[result$lag == 0]
  expect_equal(lag_0, 1, tolerance = 1e-6)
})

test_that("xcf handles different variable names", {
  df <- data.frame(
    series_a = 1:20,
    series_b = 2:21
  )
  
  result <- xcf(df, series_a, series_b)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("xcf with lagged relationship", {
  # Create series where y lags x by 1 period
  x <- 1:20
  y <- c(0, 1:19)  # Shifted by 1
  df <- data.frame(x = x, y = y)
  
  result <- xcf(df, x, y)
  
  # Should show strong correlation at positive lag
  expect_true(any(result$x.corr[result$lag > 0] > 0.8))
})
