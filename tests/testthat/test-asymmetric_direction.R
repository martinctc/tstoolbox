test_that("asymmetric_direction basic functionality works", {
  set.seed(123)
  n <- 100
  x <- cumsum(rnorm(n))
  y <- cumsum(rnorm(n))
  
  result <- asymmetric_direction(x, y, reference = "x")
  
  expect_type(result, "list")
  expect_s3_class(result, "asymmetric_direction")
  expect_named(result, c("overall", "upturn", "downturn", "asymmetry", 
                         "n_upturn", "n_downturn", "p_value", "reference", "interpretation"))
  
  # Check value ranges
  expect_gte(result$overall, 0)
  expect_lte(result$overall, 1)
  expect_gte(result$upturn, 0)
  expect_lte(result$upturn, 1)
  expect_gte(result$downturn, 0)
  expect_lte(result$downturn, 1)
  expect_gte(result$p_value, 0)
  expect_lte(result$p_value, 1)
})

test_that("asymmetric_direction detects symmetric relationship", {
  # Create symmetric relationship
  set.seed(456)
  n <- 200
  x <- cumsum(rnorm(n))
  y <- x + rnorm(n, sd = 0.5)
  
  result <- asymmetric_direction(x, y, reference = "x")
  
  # Asymmetry should be small, p-value high
  expect_lt(abs(result$asymmetry), 0.3)
  expect_gt(result$p_value, 0.05)
})

test_that("asymmetric_direction handles different reference options", {
  set.seed(789)
  x <- cumsum(rnorm(50))
  y <- cumsum(rnorm(50))
  
  result_x <- asymmetric_direction(x, y, reference = "x")
  result_y <- asymmetric_direction(x, y, reference = "y")
  result_both <- asymmetric_direction(x, y, reference = "both")
  
  expect_s3_class(result_x, "asymmetric_direction")
  expect_s3_class(result_y, "asymmetric_direction")
  expect_s3_class(result_both, "asymmetric_direction")
  
  # "both" should typically have fewer observations
  expect_lte(result_both$n_upturn + result_both$n_downturn,
             result_x$n_upturn + result_x$n_downturn)
})

test_that("asymmetric_direction validates input", {
  x <- 1:10
  y <- 1:5
  
  expect_error(asymmetric_direction(x, y), 
               "`x` and `y` must have the same length")
})

test_that("asymmetric_direction handles edge cases", {
  # Constant series
  x <- rep(5, 20)
  y <- cumsum(rnorm(20))
  
  # Should handle without error
  expect_no_error(asymmetric_direction(x, y))
  
  # Very short series
  x_short <- c(1, 2, 3)
  y_short <- c(2, 3, 4)
  
  result <- asymmetric_direction(x_short, y_short)
  expect_s3_class(result, "asymmetric_direction")
})

test_that("asymmetric_direction interpretation is informative", {
  set.seed(999)
  x <- cumsum(rnorm(100))
  y <- cumsum(rnorm(100))
  
  result <- asymmetric_direction(x, y)
  
  expect_type(result$interpretation, "character")
  expect_gt(nchar(result$interpretation), 20)
})
