test_that("direction_test binomial method works", {
  set.seed(123)
  x <- cumsum(rnorm(50))
  y <- x + rnorm(50, sd = 0.5)
  
  result <- direction_test(x, y, method = "binomial")
  
  expect_type(result, "list")
  expect_s3_class(result, "direction_test")
  expect_named(result, c("statistic", "p.value", "conf.int", "n", 
                         "n_matches", "method", "alternative", "conf_level"))
  
  # Check value ranges
  expect_gte(result$statistic, 0)
  expect_lte(result$statistic, 1)
  expect_gte(result$p.value, 0)
  expect_lte(result$p.value, 1)
  expect_length(result$conf.int, 2)
})

test_that("direction_test detects significant co-movement", {
  # Create strongly co-moving series
  set.seed(456)
  x <- cumsum(rnorm(100))
  y <- x + rnorm(100, sd = 0.1)
  
  result <- direction_test(x, y, method = "binomial")
  
  # Should have high statistic and low p-value
  expect_gt(result$statistic, 0.6)
  expect_lt(result$p.value, 0.05)
})

test_that("direction_test handles independent series", {
  set.seed(789)
  x <- rnorm(100)
  y <- rnorm(100)
  
  result <- direction_test(x, y, method = "binomial")
  
  # Statistic should be around 0.5, p-value high
  expect_gt(result$p.value, 0.1)
  expect_gt(result$statistic, 0.3)
  expect_lt(result$statistic, 0.7)
})

test_that("direction_test alternative hypotheses work", {
  x <- 1:30
  y <- 2:31
  
  result_two <- direction_test(x, y, alternative = "two.sided")
  result_greater <- direction_test(x, y, alternative = "greater")
  result_less <- direction_test(x, y, alternative = "less")
  
  expect_equal(result_two$alternative, "two.sided")
  expect_equal(result_greater$alternative, "greater")
  expect_equal(result_less$alternative, "less")
  
  # Greater should have lower p-value for perfect co-movement
  expect_lt(result_greater$p.value, result_less$p.value)
})

test_that("direction_test permutation method works", {
  skip_on_cran()  # Skip on CRAN to save time
  
  set.seed(123)
  x <- cumsum(rnorm(30))
  y <- x + rnorm(30, sd = 0.5)
  
  result <- direction_test(x, y, method = "permutation", n_sim = 100)
  
  expect_equal(result$method, "permutation")
  expect_type(result$p.value, "double")
  expect_gte(result$p.value, 0)
  expect_lte(result$p.value, 1)
})

test_that("direction_test bootstrap method works", {
  skip_on_cran()  # Skip on CRAN to save time
  
  set.seed(456)
  x <- cumsum(rnorm(30))
  y <- x + rnorm(30, sd = 0.5)
  
  result <- direction_test(x, y, method = "bootstrap", n_sim = 100)
  
  expect_equal(result$method, "bootstrap")
  expect_length(result$conf.int, 2)
  expect_lt(result$conf.int[1], result$statistic)
  expect_gt(result$conf.int[2], result$statistic)
})

test_that("direction_test validates input lengths", {
  x <- 1:10
  y <- 1:9
  
  expect_error(
    direction_test(x, y),
    "same length"
  )
})

test_that("direction_test confidence level works", {
  x <- 1:20
  y <- 2:21
  
  result_95 <- direction_test(x, y, conf_level = 0.95)
  result_99 <- direction_test(x, y, conf_level = 0.99)
  
  # 99% CI should be wider than 95% CI
  width_95 <- result_95$conf.int[2] - result_95$conf.int[1]
  width_99 <- result_99$conf.int[2] - result_99$conf.int[1]
  expect_gt(width_99, width_95)
})

test_that("direction_test print method exists", {
  x <- 1:20
  y <- 2:21
  
  result <- direction_test(x, y)
  
  # Test that print doesn't error
  expect_output(print(result))
})
