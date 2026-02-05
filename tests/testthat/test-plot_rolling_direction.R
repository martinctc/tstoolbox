test_that("plot_rolling_direction creates a plot", {
  set.seed(123)
  x <- cumsum(rnorm(100))
  y <- x + rnorm(100, sd = 0.5)
  
  p <- plot_rolling_direction(x, y, window = 20)
  
  expect_s3_class(p, "ggplot")
  expect_true(!is.null(p$data))
})

test_that("plot_rolling_direction handles different window sizes", {
  set.seed(456)
  x <- cumsum(rnorm(100))
  y <- cumsum(rnorm(100))
  
  p_small <- plot_rolling_direction(x, y, window = 10)
  p_large <- plot_rolling_direction(x, y, window = 30)
  
  expect_s3_class(p_small, "ggplot")
  expect_s3_class(p_large, "ggplot")
  
  # Larger window should have fewer data points
  expect_lt(nrow(p_large$data), nrow(p_small$data))
})

test_that("plot_rolling_direction validates input", {
  x <- 1:10
  y <- 1:5
  
  expect_error(plot_rolling_direction(x, y, window = 5), 
               "`x` and `y` must have the same length")
})

test_that("plot_rolling_direction handles minimum window size", {
  x <- 1:20
  y <- 2:21
  
  # Window size 2 should work (minimum for direction calculation)
  expect_no_error(plot_rolling_direction(x, y, window = 3))
  
  # Window size too large should work (may produce warnings about empty data)
  expect_no_error(suppressWarnings(plot_rolling_direction(x, y, window = 25)))
})

test_that("plot_rolling_direction produces expected data structure", {
  set.seed(789)
  x <- cumsum(rnorm(50))
  y <- cumsum(rnorm(50))
  
  p <- plot_rolling_direction(x, y, window = 15)
  
  # Check that data has expected columns
  expect_true(all(c("time", "comovement") %in% names(p$data)))
  
  # Proportions should be between 0 and 1 (excluding NAs)
  expect_true(all(p$data$comovement[!is.na(p$data$comovement)] >= 0 & 
                  p$data$comovement[!is.na(p$data$comovement)] <= 1))
})

test_that("plot_rolling_direction works with data frames", {
  set.seed(111)
  df <- data.frame(
    x = cumsum(rnorm(80)),
    y = cumsum(rnorm(80))
  )
  
  p <- plot_rolling_direction(df$x, df$y, window = 20)
  expect_s3_class(p, "ggplot")
})

test_that("plot_rolling_direction handles edge cases", {
  # Constant series
  x <- rep(5, 30)
  y <- cumsum(rnorm(30))
  
  expect_no_error(plot_rolling_direction(x, y, window = 10))
  
  # Series with NA values
  x <- c(1:20, NA, 22:30)
  y <- c(NA, 2:30)
  
  # Should handle NAs appropriately
  expect_no_error(plot_rolling_direction(x, y, window = 10))
})
