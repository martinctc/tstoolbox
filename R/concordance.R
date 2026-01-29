#' Calculate concordance index for co-movement
#'
#' Computes formal concordance measures used in economics and finance
#' to quantify how two time series move together.
#'
#' @param x Numeric vector for the first time series.
#' @param y Numeric vector for the second time series (same length as `x`).
#' @param method Character string specifying the concordance method:
#'   "harding-pagan" (default) or "simple".
#'
#' @return A list with class "concordance" containing:
#'   \item{concordance}{The concordance index (0 to 1).}
#'   \item{expected}{Expected concordance under independence (0.5 for balanced series).}
#'   \item{adjusted}{Concordance adjusted for expected value, ranging from -1 to 1.}
#'   \item{n}{Number of valid observations.}
#'   \item{method}{The method used.}
#'   \item{p_positive}{Proportion of positive changes in each series.}
#'
#' @details
#' The Harding-Pagan concordance index measures the proportion of time
#' two series are in the same state (both expanding or both contracting):
#'
#' \deqn{CI = \frac{1}{T}\sum_{t=1}^{T}[S_{x,t} \cdot S_{y,t} + (1-S_{x,t})(1-S_{y,t})]}
#'
#' where \eqn{S_{i,t} = 1} if series i is in expansion at time t.
#'
#' The adjusted concordance transforms this to a -1 to 1 scale:
#' \itemize{
#'   \item 1: Perfect positive co-movement
#'   \item 0: Independence (no systematic relationship)
#'   \item -1: Perfect negative co-movement (counter-cyclical)
#' }
#'
#' @references
#' Harding, D., & Pagan, A. (2002). Dissecting the cycle: a methodological
#' investigation. Journal of Monetary Economics, 49(2), 365-381.
#'
#' @examples
#' # Strongly co-moving series
#' set.seed(123)
#' x <- cumsum(rnorm(100))
#' y <- x + rnorm(100, sd = 0.5)
#' concordance(x, y)
#'
#' # Independent series
#' y_indep <- cumsum(rnorm(100))
#' concordance(x, y_indep)
#'
#' # Counter-cyclical series
#' y_counter <- -x + rnorm(100, sd = 0.5)
#' concordance(x, y_counter)
#'
#' @export
concordance <- function(x, y, method = c("harding-pagan", "simple")) {
  
  method <- match.arg(method)
  
  # Input validation
  if (length(x) != length(y)) {
    stop("`x` and `y` must have the same length.")
  }
  
  # Get directions as binary (1 = positive/expansion, 0 = negative/contraction)
  dir_x <- direction(x)
  dir_y <- direction(y)
  
  # Convert to binary states
  state_x <- ifelse(dir_x == "Positive", 1, 0)
  state_y <- ifelse(dir_y == "Positive", 1, 0)
  
  # Remove NAs
  valid <- !is.na(state_x) & !is.na(state_y)
  state_x <- state_x[valid]
  state_y <- state_y[valid]
  n <- length(state_x)
  
  if (n == 0) {
    stop("No valid observations for concordance calculation.")
  }
  
  # Calculate proportions of positive states
  p_x <- mean(state_x)
  p_y <- mean(state_y)
  
  if (method == "harding-pagan") {
    # Harding-Pagan concordance index
    concordance_idx <- mean(
      state_x * state_y + (1 - state_x) * (1 - state_y)
    )
    
    # Expected concordance under independence
    expected <- p_x * p_y + (1 - p_x) * (1 - p_y)
    
  } else {
    # Simple proportion of matching directions
    concordance_idx <- mean(state_x == state_y)
    expected <- 0.5
  }
  
  # Adjusted concordance: transform to [-1, 1] scale
  # Maps [0, 1] to [-1, 1] where expected maps to 0
  if (expected == 0 || expected == 1) {
    adjusted <- NA_real_
  } else {
    adjusted <- (concordance_idx - expected) / (min(expected, 1 - expected))
    adjusted <- max(-1, min(1, adjusted))  
  }
  
  # Build result
  result <- list(
    concordance = concordance_idx,
    expected = expected,
    adjusted = adjusted,
    n = n,
    method = method,
    p_positive = c(x = p_x, y = p_y)
  )
  
  class(result) <- "concordance"
  result
}

#' @export
print.concordance <- function(x, ...) {
  cat("\n\tConcordance Index (", x$method, ")\n\n", sep = "")
  cat("Concordance:", sprintf("%.4f", x$concordance), "\n")
  cat("Expected (under independence):", sprintf("%.4f", x$expected), "\n")
  cat("Adjusted concordance:", sprintf("%.4f", x$adjusted), "\n\n")
  cat("Observations:", x$n, "\n")
  cat("Proportion positive - x:", sprintf("%.1f%%", x$p_positive["x"] * 100), "\n")
  cat("Proportion positive - y:", sprintf("%.1f%%", x$p_positive["y"] * 100), "\n")
  invisible(x)
}
