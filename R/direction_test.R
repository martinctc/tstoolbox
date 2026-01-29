#' Test statistical significance of co-movement between two time series
#'
#' Tests whether the observed co-movement proportion between two time series
#' is statistically different from what would be expected by chance (50%).
#'
#' @param x Numeric vector for the first time series.
#' @param y Numeric vector for the second time series (same length as `x`).
#' @param method Character string specifying the test method: "binomial" (default),
#'   "permutation", or "bootstrap".
#' @param alternative Character string specifying the alternative hypothesis:
#'   "two.sided" (default), "greater", or "less".
#' @param n_sim Number of simulations for permutation or bootstrap tests.
#'   Defaults to 1000.
#' @param conf_level Confidence level for the confidence interval. Defaults to 0.95.
#'
#' @return A list with class "direction_test" containing:
#'   \item{statistic}{The observed co-movement proportion.}
#'   \item{p.value}{The p-value for the test.}
#'   \item{conf.int}{Confidence interval for the co-movement proportion.}
#'   \item{n}{Number of valid direction pairs.}
#'   \item{n_matches}{Number of matching direction pairs.}
#'   \item{method}{The test method used.}
#'   \item{alternative}{The alternative hypothesis.}
#'
#' @details
#' Under the null hypothesis, two independent series have a 50% chance of

#' moving in the same direction in any given period.
#'
#' \describe{
#'   \item{binomial}{Exact binomial test against p = 0.5. Fast and appropriate
#'     for most cases.}
#'   \item{permutation}{Randomly shuffles one series to create null distribution.
#'     Non-parametric but computationally intensive.}
#'   \item{bootstrap}{Resamples the direction matches to estimate uncertainty.
#'     Useful for confidence intervals.}
#' }
#'
#' @examples
#' # Test co-movement of two related series
#' set.seed(42)
#' x <- cumsum(rnorm(50))
#' y <- x + rnorm(50, sd = 0.3)
#' direction_test(x, y)
#'
#' # Test with permutation method
#' direction_test(x, y, method = "permutation", n_sim = 500)
#'
#' # One-sided test (do they move together more than chance?)
#' direction_test(x, y, alternative = "greater")
#'
#' @export
direction_test <- function(x, y, 
                           method = c("binomial", "permutation", "bootstrap"),
                           alternative = c("two.sided", "greater", "less"),
                           n_sim = 1000,
                           conf_level = 0.95) {
  
 method <- match.arg(method)
  alternative <- match.arg(alternative)
  
  # Input validation
  if (length(x) != length(y)) {
    stop("`x` and `y` must have the same length.")
  }
  
  # Get directions
  dir_x <- direction(x)
  dir_y <- direction(y)
  
  # Find matches (excluding NAs)
  valid_idx <- !is.na(dir_x) & !is.na(dir_y)
  matches <- dir_x[valid_idx] == dir_y[valid_idx]
  
  n <- sum(valid_idx)
  n_matches <- sum(matches)
  observed_prop <- n_matches / n
  
  # Perform test based on method
  if (method == "binomial") {
    test_result <- stats::binom.test(
      n_matches, n, 
      p = 0.5, 
      alternative = alternative,
      conf.level = conf_level
    )
    p_value <- test_result$p.value
    conf_int <- test_result$conf.int
    
  } else if (method == "permutation") {
    # Permutation test
    null_dist <- replicate(n_sim, {
      shuffled_y <- sample(dir_y[valid_idx])
      sum(dir_x[valid_idx] == shuffled_y) / n
    })
    
    p_value <- switch(
      alternative,
      "two.sided" = 2 * min(mean(null_dist >= observed_prop), 
                           mean(null_dist <= observed_prop)),
      "greater" = mean(null_dist >= observed_prop),
      "less" = mean(null_dist <= observed_prop)
    )
    p_value <- min(p_value, 1)
    
    # Bootstrap CI
    boot_dist <- replicate(n_sim, {
      boot_idx <- sample(seq_len(n), replace = TRUE)
      mean(matches[boot_idx])
    })
    alpha <- 1 - conf_level
    conf_int <- stats::quantile(boot_dist, c(alpha/2, 1 - alpha/2))
    
  } else {
    # Bootstrap test
    boot_dist <- replicate(n_sim, {
      boot_idx <- sample(seq_len(n), replace = TRUE)
      mean(matches[boot_idx])
    })
    
    # P-value by comparing to 0.5
    centered_dist <- boot_dist - observed_prop + 0.5
    p_value <- switch(
      alternative,
      "two.sided" = 2 * min(mean(centered_dist >= observed_prop), 
                           mean(centered_dist <= observed_prop)),
      "greater" = mean(centered_dist >= observed_prop),
      "less" = mean(centered_dist <= observed_prop)
    )
    p_value <- min(p_value, 1)
    
    alpha <- 1 - conf_level
    conf_int <- stats::quantile(boot_dist, c(alpha/2, 1 - alpha/2))
  }
  
  # Build result
  result <- list(
    statistic = observed_prop,
    p.value = p_value,
    conf.int = conf_int,
    n = n,
    n_matches = n_matches,
    method = method,
    alternative = alternative,
    conf_level = conf_level
  )
  
  class(result) <- "direction_test"
  result
}

#' @export
print.direction_test <- function(x, ...) {
  cat("\n\tDirectional Co-movement Test\n\n")
  cat("Method:", x$method, "\n")
  cat("Alternative:", x$alternative, "\n\n")
  cat("Co-movement proportion:", round(x$statistic, 4), "\n")
  cat("Matches:", x$n_matches, "out of", x$n, "periods\n")
  cat(sprintf("%d%% CI: [%.4f, %.4f]\n", 
              round(x$conf_level * 100), x$conf.int[1], x$conf.int[2]))
  cat("p-value:", format.pval(x$p.value, digits = 4), "\n")
  invisible(x)
}
