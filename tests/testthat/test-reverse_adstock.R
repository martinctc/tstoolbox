test_that("reverse_adstock basic functionality works", {
  original <- c(100, 200, 300, 150, 200)
  rate <- 0.2
  
  adstocked <- adstock(original, rate = rate)
  reversed <- reverse_adstock(adstocked, rate = rate)
  
  expect_type(reversed, "double")
  expect_length(reversed, length(original))
  
  # Should recover original values (within floating point tolerance)
  expect_equal(reversed, original, tolerance = 1e-10)
})

test_that("reverse_adstock works with different rates", {
  original <- c(50, 100, 150, 75, 125, 200)
  
  for (rate in c(0, 0.1, 0.3, 0.5, 0.7, 0.9)) {
    adstocked <- adstock(original, rate = rate)
    reversed <- reverse_adstock(adstocked, rate = rate)
    
    expect_equal(reversed, original, tolerance = 1e-10,
                 info = paste("Failed for rate =", rate))
  }
})

test_that("reverse_adstock preserves first value", {
  original <- c(42, 100, 200, 300)
  rate <- 0.4
  
  adstocked <- adstock(original, rate = rate)
  reversed <- reverse_adstock(adstocked, rate = rate)
  
  # First value should always be preserved exactly
  expect_equal(reversed[1], original[1])
})

test_that("reverse_adstock with rate 0 returns input unchanged", {
  x <- c(10, 20, 30, 40, 50)
  
  result <- reverse_adstock(x, rate = 0)
  
  # With rate 0, reverse should return original (no adstock effect)
  expect_equal(result, x)
})

test_that("reverse_adstock handles single value", {
  x <- 100
  rate <- 0.5
  
  result <- reverse_adstock(x, rate = rate)
  
  expect_length(result, 1)
  expect_equal(result, x)
})

test_that("reverse_adstock handles negative values", {
  original <- c(100, -50, 75, -25, 50)
  rate <- 0.3
  
  adstocked <- adstock(original, rate = rate)
  reversed <- reverse_adstock(adstocked, rate = rate)
  
  expect_equal(reversed, original, tolerance = 1e-10)
})

test_that("reverse_adstock handles zeros", {
  original <- c(100, 0, 50, 0, 25)
  rate <- 0.4
  
  adstocked <- adstock(original, rate = rate)
  reversed <- reverse_adstock(adstocked, rate = rate)
  
  expect_equal(reversed, original, tolerance = 1e-10)
})

test_that("reverse_adstock round-trip consistency", {
  # Test multiple round trips
  original <- c(10, 25, 40, 15, 30, 50)
  rate <- 0.35
  
  # Forward and back multiple times
  x <- original
  for (i in 1:5) {
    x <- adstock(x, rate = rate)
    x <- reverse_adstock(x, rate = rate)
  }
  
  expect_equal(x, original, tolerance = 1e-8)
})

test_that("reverse_adstock works with realistic marketing data", {
  # Simulate weekly marketing spend
  spend <- c(1000, 1500, 2000, 1200, 800, 1800, 2200)
  rate <- 0.6  # 60% carryover
  
  adstocked_spend <- adstock(spend, rate = rate)
  recovered_spend <- reverse_adstock(adstocked_spend, rate = rate)
  
  expect_equal(recovered_spend, spend, tolerance = 1e-10)
})

test_that("reverse_adstock handles edge case rates", {
  original <- c(100, 200, 150, 250)
  
  # Very high rate (near 1)
  rate_high <- 0.99
  adstocked_high <- adstock(original, rate = rate_high)
  reversed_high <- reverse_adstock(adstocked_high, rate = rate_high)
  expect_equal(reversed_high, original, tolerance = 1e-8)
  
  # Very low rate (near 0)
  rate_low <- 0.01
  adstocked_low <- adstock(original, rate = rate_low)
  reversed_low <- reverse_adstock(adstocked_low, rate = rate_low)
  expect_equal(reversed_low, original, tolerance = 1e-10)
})
