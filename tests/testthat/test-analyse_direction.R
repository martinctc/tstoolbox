test_that("analyse_direction calculates co-movement correctly", {
  df <- data.frame(
    x = c(1, 3, 2, 5, 4, 7),
    y = c(2, 4, 3, 6, 5, 8)
  )
  
  # Suppress the message output for testing
  result <- suppressMessages(analyse_direction(df, x, y))
  
  expect_s3_class(result, "data.frame")
  expect_named(result, c("n", "base", "prop"))
  expect_equal(result$n, 5)    # All 5 changes match
  expect_equal(result$base, 5) # 5 valid comparisons
  expect_equal(result$prop, 1) # 100% match
})

test_that("analyse_direction handles perfect co-movement", {
  df <- data.frame(
    x = 1:10,
    y = 2:11
  )
  
  result <- suppressMessages(analyse_direction(df, x, y))
  
  expect_equal(result$n, result$base)
  expect_equal(result$prop, 1)
})

test_that("analyse_direction handles opposite movements", {
  df <- data.frame(
    x = 1:10,
    y = 10:1
  )
  
  result <- suppressMessages(analyse_direction(df, x, y))
  
  expect_equal(result$n, 0)     # No matches
  expect_equal(result$prop, 0)  # 0% match
})

test_that("analyse_direction handles mixed movements", {
  df <- data.frame(
    x = c(1, 3, 2, 5, 4),
    y = c(2, 1, 3, 4, 6)  # Different directions
  )
  
  result <- suppressMessages(analyse_direction(df, x, y))
  
  expect_gte(result$prop, 0)
  expect_lte(result$prop, 1)
  expect_equal(result$n + (result$base - result$n), result$base)
})

test_that("analyse_direction message output is correct", {
  df <- data.frame(
    x = c(1, 3, 2, 5, 4, 7),
    y = c(2, 4, 3, 6, 5, 8)
  )
  
  expect_message(
    analyse_direction(df, x, y),
    "There are 5 out of 5 instance\\(s\\) \\(100%\\)"
  )
})

test_that("analyse_direction handles data with NAs", {
  df <- data.frame(
    x = c(1, 3, NA, 5, 4),
    y = c(2, 4, 3, 6, 5)
  )
  
  result <- suppressMessages(analyse_direction(df, x, y))
  
  # Should drop NA rows
  expect_lt(result$base, 4)  # Less than 4 comparisons (5 values - 1 for lag)
})

test_that("analyse_direction works with different variable names", {
  df <- data.frame(
    series_a = c(1, 3, 2, 5),
    series_b = c(2, 4, 3, 6)
  )
  
  result <- suppressMessages(analyse_direction(df, series_a, series_b))
  
  expect_type(result$n, "integer")
  expect_type(result$base, "integer")
  expect_type(result$prop, "double")
})

test_that("analyse_direction handles minimum data", {
  df <- data.frame(
    x = c(1, 3),
    y = c(2, 4)
  )
  
  result <- suppressMessages(analyse_direction(df, x, y))
  
  expect_equal(result$base, 1)  # Only 1 comparison possible
})
