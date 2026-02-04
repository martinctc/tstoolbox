#!/usr/bin/env Rscript

# Script to run package tests
# Usage: Rscript run_tests.R

cat("Running tstoolbox test suite...\n\n")

# Check if required packages are installed
required_pkgs <- c("testthat", "devtools")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  cat("Installing required packages:", paste(missing_pkgs, collapse = ", "), "\n")
  install.packages(missing_pkgs, repos = "https://cran.r-project.org")
}

# Load packages
library(testthat)
library(devtools)

# Run tests
cat("\n=== Running Tests ===\n\n")
test_results <- devtools::test()

# Summary
cat("\n=== Test Summary ===\n")
cat("Tests run:", sum(test_results$nb), "\n")
cat("Failures:", test_results$failed, "\n")
cat("Warnings:", test_results$warning, "\n")
cat("Skipped:", test_results$skipped, "\n")

# Exit with appropriate code
if (test_results$failed > 0) {
  cat("\n❌ Some tests failed!\n")
  quit(status = 1)
} else {
  cat("\n✓ All tests passed!\n")
  quit(status = 0)
}
