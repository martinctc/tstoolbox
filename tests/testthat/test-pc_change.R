test_that("pc_change calculates percentage change correctly", {
  x <- c(100, 110, 121, 100)
  result <- pc_change(x)
  
  expect_type(result, "double")
  expect_length(result, length(x))
  expect_true(is.na(result[1]))
  expect_equal(result[2], 0.1)     # (110-100)/100 = 0.1 (10%)
  expect_equal(result[3], 0.1)     # (121-110)/110 = 0.1 (10%)
  expect_equal(result[4], -0.173553719, tolerance = 1e-6)  # (100-121)/121
})

test_that("pc_change with lag = 2", {
  x <- c(100, 110, 121, 133.1)
  result <- pc_change(x, lag = 2)
  
  expect_length(result, length(x))
  expect_true(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_equal(result[3], 0.21)     # (121-100)/100 = 0.21 (21%)
  expect_equal(result[4], 0.21, tolerance = 1e-6)  # (133.1-110)/110
})

test_that("pc_change handles negative values", {
  x <- c(100, 50, 25)
  result <- pc_change(x)
  
  expect_equal(result[2], -0.5)    # -50%
  expect_equal(result[3], -0.5)    # -50%
})

test_that("pc_change handles zero base value", {
  x <- c(0, 10, 20)
  result <- pc_change(x)
  
  expect_true(is.na(result[1]))
  expect_true(is.infinite(result[2]))  # Division by zero
})

test_that("pc_change with single value", {
  x <- 100
  result <- pc_change(x)
  
  expect_length(result, 1)
  expect_true(is.na(result[1]))
})

test_that("pc_change with all equal values", {
  x <- c(100, 100, 100, 100)
  result <- pc_change(x)
  
  expect_equal(result[2:4], rep(0, 3))
})

test_that("pc_change handles NA values", {
  x <- c(100, NA, 121, 100)
  result <- pc_change(x)
  
  expect_length(result, 4)
  expect_true(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_true(is.na(result[3]))  # Can't calculate with NA
})

test_that("pc_change with large lag", {
  x <- 1:10
  result <- pc_change(x, lag = 5)
  
  expect_length(result, 10)
  expect_true(all(is.na(result[1:5])))
  expect_false(is.na(result[6]))
})

test_that("pc_change handles decimal values", {
  x <- c(1.5, 1.65, 1.815)
  result <- pc_change(x)
  
  expect_equal(result[2], 0.1, tolerance = 1e-6)
  expect_equal(result[3], 0.1, tolerance = 1e-6)
})

test_that("pc_change with lag = 0 should error or handle appropriately", {
  x <- c(100, 110, 121)
  
  # lag = 0 might cause issues - test current behavior
  # If it errors, that's fine; if not, document the behavior
  expect_error(pc_change(x, lag = 0))
})
