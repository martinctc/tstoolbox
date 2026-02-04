test_that("direction returns correct direction values", {
  # Basic functionality
  x <- c(1, 3, 2, 5, 5, 4)
  result <- direction(x)
  
  expect_type(result, "character")
  expect_length(result, length(x))
  expect_equal(result[1], NA_character_)  # First value is NA (no previous value)
  expect_equal(result[2], "Positive")     # 3 > 1
  expect_equal(result[3], "Negative")     # 2 < 3
  expect_equal(result[4], "Positive")     # 5 > 2
  expect_equal(result[5], "Equal")        # 5 == 5
  expect_equal(result[6], "Negative")     # 4 < 5
})

test_that("direction handles all equal values", {
  x <- c(5, 5, 5, 5)
  result <- direction(x)
  
  expect_equal(result[2:4], rep("Equal", 3))
})

test_that("direction handles strictly increasing series", {
  x <- 1:10
  result <- direction(x)
  
  expect_equal(result[2:10], rep("Positive", 9))
})

test_that("direction handles strictly decreasing series", {
  x <- 10:1
  result <- direction(x)
  
  expect_equal(result[2:10], rep("Negative", 9))
})

test_that("direction handles single value", {
  x <- 5
  result <- direction(x)
  
  expect_length(result, 1)
  expect_true(is.na(result[1]))
})

test_that("direction handles two values", {
  x <- c(1, 3)
  result <- direction(x)
  
  expect_length(result, 2)
  expect_equal(result[1], NA_character_)
  expect_equal(result[2], "Positive")
})

test_that("direction handles NA values in input", {
  x <- c(1, 3, NA, 5, 4)
  result <- direction(x)
  
  expect_length(result, 5)
  expect_equal(result[1], NA_character_)
  expect_equal(result[2], "Positive")
  expect_equal(result[3], NA_character_)  # NA - 3
  expect_equal(result[4], NA_character_)  # 5 - NA
})

test_that("direction handles negative numbers", {
  x <- c(-5, -2, -8, 0, 3)
  result <- direction(x)
  
  expect_equal(result[2], "Positive")   # -2 > -5
  expect_equal(result[3], "Negative")   # -8 < -2
  expect_equal(result[4], "Positive")   # 0 > -8
  expect_equal(result[5], "Positive")   # 3 > 0
})

test_that("direction handles very small differences", {
  x <- c(1.0000, 1.0001, 1.0000)
  result <- direction(x)
  
  expect_equal(result[2], "Positive")
  expect_equal(result[3], "Negative")
})
