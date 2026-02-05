test_that("adstock applies decay correctly", {
  x <- c(100, 0, 0, 0, 0)
  
  # With 50% decay rate
  result <- adstock(x, rate = 0.5)
  
  expect_type(result, "double")
  expect_length(result, length(x))
  expect_equal(result[1], 100)
  expect_equal(result[2], 50)    # 100 * 0.5
  expect_equal(result[3], 25)    # 50 * 0.5
  expect_equal(result[4], 12.5)  # 25 * 0.5
  expect_equal(result[5], 6.25)  # 12.5 * 0.5
})

test_that("adstock with zero rate returns original", {
  x <- c(100, 200, 300)
  result <- adstock(x, rate = 0)
  
  expect_equal(result, x)
})

test_that("adstock handles multiple inputs", {
  x <- c(100, 200, 300, 150, 200)
  result <- adstock(x, rate = 0.2)
  
  expect_type(result, "double")
  expect_length(result, 5)
  expect_equal(result[1], 100)
  expect_equal(result[2], 220)  # 100*0.2 + 200
  expect_gt(result[3], 300)     # Should have carryover
})

test_that("adstock works with rate close to 1", {
  x <- c(100, 0, 0, 0)
  result <- adstock(x, rate = 0.9)
  
  expect_equal(result[1], 100)
  expect_equal(result[2], 90)   # 100 * 0.9
  expect_equal(result[3], 81)   # 90 * 0.9
  expect_equal(result[4], 72.9) # 81 * 0.9
})

test_that("adstock handles single value", {
  x <- 100
  result <- adstock(x, rate = 0.5)
  
  expect_length(result, 1)
  expect_equal(result[1], 100)
})

test_that("adstock handles all zeros", {
  x <- rep(0, 5)
  result <- adstock(x, rate = 0.5)
  
  expect_equal(result, rep(0, 5))
})

test_that("adstock handles negative values", {
  x <- c(100, -50, 0, 0)
  result <- adstock(x, rate = 0.5)
  
  expect_equal(result[1], 100)
  expect_equal(result[2], 0)   # 100*0.5 + (-50) = 0
  expect_equal(result[3], 0)   # 0*0.5 + 0 = 0
})

test_that("adstock with NA values", {
  x <- c(100, NA, 200)
  result <- adstock(x, rate = 0.5)
  
  expect_length(result, 3)
  expect_equal(result[1], 100)
  expect_true(is.na(result[2]))
})
