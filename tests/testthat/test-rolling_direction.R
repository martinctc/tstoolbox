test_that("rolling_direction calculates rolling co-movement", {
  set.seed(123)
  x <- cumsum(rnorm(50))
  y <- x + rnorm(50, sd = 0.5)
  
  result <- rolling_direction(x, y, window = 10)
  
  expect_type(result, "double")
  expect_length(result, length(x))
  
  # Check that values are proportions
  valid_values <- result[!is.na(result)]
  expect_true(all(valid_values >= 0 & valid_values <= 1))
})

test_that("rolling_direction alignment works correctly", {
  x <- 1:20
  y <- 2:21
  window <- 5
  
  result_right <- rolling_direction(x, y, window = window, align = "right")
  result_left <- rolling_direction(x, y, window = window, align = "left")
  result_center <- rolling_direction(x, y, window = window, align = "center")
  
  # Right aligned: first window-1 values should be NA
  expect_true(all(is.na(result_right[1:(window-1)])))
  expect_false(is.na(result_right[window]))
  
  # Left aligned: last window-1 values should be NA
  expect_true(all(is.na(result_left[(length(x)-window+2):length(x)])))
  expect_false(is.na(result_left[1]))
  
  # All should be same length
  expect_length(result_right, length(x))
  expect_length(result_left, length(x))
  expect_length(result_center, length(x))
})

test_that("rolling_direction validates input lengths", {
  x <- 1:10
  y <- 1:9
  
  expect_error(
    rolling_direction(x, y, window = 5),
    "same length"
  )
})

test_that("rolling_direction validates window size", {
  x <- 1:10
  y <- 2:11
  
  expect_error(
    rolling_direction(x, y, window = 1),
    "at least 2"
  )
})

test_that("rolling_direction validates align parameter", {
  x <- 1:10
  y <- 2:11
  
  expect_error(
    rolling_direction(x, y, window = 3, align = "middle"),
    "one of 'right', 'center', or 'left'"
  )
})

test_that("rolling_direction handles perfect co-movement", {
  x <- 1:30
  y <- 2:31
  
  result <- rolling_direction(x, y, window = 10)
  
  # All valid values should be 1 (perfect co-movement)
  valid_values <- result[!is.na(result)]
  expect_true(all(valid_values == 1))
})

test_that("rolling_direction handles opposite movements", {
  x <- 1:30
  y <- 30:1
  
  result <- rolling_direction(x, y, window = 10)
  
  # All valid values should be 0 (opposite movement)
  valid_values <- result[!is.na(result)]
  expect_true(all(valid_values == 0))
})

test_that("rolling_direction handles min_obs parameter", {
  x <- c(1, 3, 2, 5, NA, 4, 7, 6, 9, 8)
  y <- c(2, 4, 3, 6, 5, 7, 8, 9, 10, 11)
  
  result <- rolling_direction(x, y, window = 5, min_obs = 3)
  
  expect_length(result, length(x))
})

test_that("rolling_direction with small window", {
  x <- 1:10
  y <- 2:11
  
  result <- rolling_direction(x, y, window = 2)
  
  expect_length(result, 10)
  expect_true(is.na(result[1]))
  expect_false(is.na(result[2]))
})
