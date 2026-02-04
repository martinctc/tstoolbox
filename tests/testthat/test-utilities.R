# Test utility functions

test_that("package loads without errors", {
  expect_true(require(tstoolbox))
})

test_that("pipe operator is available", {
  # Test that %>% is exported
  expect_true(exists("%>%"))
  
  # Test basic pipe functionality
  result <- c(1, 2, 3) %>% sum()
  expect_equal(result, 6)
})

test_that("all documented functions are exported", {
  # Get list of functions from NAMESPACE (exported functions)
  # This helps ensure documentation matches exports
  
  # Key functions that should be exported
  expected_exports <- c(
    "direction",
    "analyse_direction",
    "concordance",
    "direction_test",
    "rolling_direction",
    "plot_rolling_direction",
    "direction_leadlag",
    "asymmetric_direction",
    "xcf",
    "plot_xcf",
    "adstock",
    "reverse_adstock",
    "pc_change"
  )
  
  for (func in expected_exports) {
    expect_true(
      exists(func, where = "package:tstoolbox", mode = "function"),
      info = paste(func, "should be exported")
    )
  }
})
