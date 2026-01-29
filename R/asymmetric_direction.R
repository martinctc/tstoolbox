#' Analyse asymmetric co-movement between two time series
#'
#' Tests whether two time series co-move differently during upturns versus
#' downturns. This is important for understanding if relationships change
#' during different market conditions.
#'
#' @param x Numeric vector for the first time series.
#' @param y Numeric vector for the second time series (same length as `x`).
#' @param reference Which series to use for defining upturns/downturns:
#'   "x" (default), "y", or "both" (consensus of both series).
#'
#' @return A list with class "asymmetric_direction" containing:
#'   \item{overall}{Overall co-movement proportion.}
#'   \item{upturn}{Co-movement proportion during upturns.}
#'   \item{downturn}{Co-movement proportion during downturns.}
#'   \item{asymmetry}{Difference between upturn and downturn co-movement.}
#'   \item{n_upturn}{Number of upturn periods.}
#'   \item{n_downturn}{Number of downturn periods.}
#'   \item{p_value}{P-value testing if asymmetry is significant (chi-squared test).}
#'   \item{interpretation}{Human-readable interpretation.}
#'
#' @details
#' Asymmetric co-movement occurs when series move together more strongly
#' during one phase (expansion or contraction) than another. This is common
#' in financial markets where correlations often increase during downturns
#' ("correlations go to 1 in a crisis").
#'
#' The function:
#' 1
#' 2. Calculates co-movement proportion separately for each phase
#' 3. Tests for significant difference using a chi-squared test
#'
#' @examples
#' # Simulate asymmetric relationship
#' set.seed(42)
#' n <- 200
#' x <- cumsum(rnorm(n))
#' 
#' # y follows x closely in downturns, loosely in upturns
#' y <- numeric(n)
#' for (i in 2:n) {
#'   if (x[i] < x[i-1]) {
#'     y[i] <- y[i-1] + (x[i] - x[i-1]) + rnorm(1, sd = 0.1)
#'   } else {
#'     y[i] <- y[i-1] + rnorm(1, sd = 1)
#'   }
#' }
#' 
#' result <- asymmetric_direction(x, y)
#' print(result)
#'
#' @export
asymmetric_direction <- function(x, y, reference = c("x", "y", "both")) {
  
  reference <- match.arg(reference)
  
  # Input validation
  if (length(x) != length(y)) {
    stop("`x` and `y` must have the same length.")
  }
  
  # Get directions
  dir_x <- direction(x)
  dir_y <- direction(y)
  
  # Determine reference state for upturn/downturn
  if (reference == "x") {
    ref_state <- dir_x
  } else if (reference == "y") {
    ref_state <- dir_y
  } else {
    # Both: consensus (both must agree)
    ref_state <- ifelse(dir_x == dir_y, dir_x, NA)
  }
  
  # Find valid observations
  valid <- !is.na(dir_x) & !is.na(dir_y) & !is.na(ref_state)
  
  dir_x <- dir_x[valid]
  dir_y <- dir_y[valid]
  ref_state <- ref_state[valid]
  
  # Split into upturn and downturn periods
  upturn_idx <- ref_state == "Positive"
  downturn_idx <- ref_state == "Negative"
  
  n_upturn <- sum(upturn_idx)
  n_downturn <- sum(downturn_idx)
  
  # Calculate co-movement in each phase
  if (n_upturn > 0) {
    matches_upturn <- sum(dir_x[upturn_idx] == dir_y[upturn_idx])
    comovement_upturn <- matches_upturn / n_upturn
  } else {
    matches_upturn <- 0
    comovement_upturn <- NA_real_
  }
  
  if (n_downturn > 0) {
    matches_downturn <- sum(dir_x[downturn_idx] == dir_y[downturn_idx])
    comovement_downturn <- matches_downturn / n_downturn
  } else {
    matches_downturn <- 0
    comovement_downturn <- NA_real_
  }
  
  # Overall co-movement
  overall <- sum(dir_x == dir_y) / length(dir_x)
  
  # Asymmetry measure
  asymmetry <- comovement_upturn - comovement_downturn
  
  # Chi-squared test for significance of asymmetry
  if (n_upturn > 0 && n_downturn > 0) {
    contingency_table <- matrix(
      c(matches_upturn, n_upturn - matches_upturn,
        matches_downturn, n_downturn - matches_downturn),
      nrow = 2, byrow = TRUE
    )
    
    # Suppress warnings for small expected values
    chi_test <- suppressWarnings(
      stats::chisq.test(contingency_table, correct = TRUE)
    )
    p_value <- chi_test$p.value
  } else {
    p_value <- NA_real_
  }
  
  # Interpretation
  if (is.na(asymmetry)) {
    interpretation <- "Insufficient data for asymmetry analysis."
  } else if (abs(asymmetry) < 0.05) {
    interpretation <- "Symmetric co-movement (similar in upturns and downturns)."
  } else if (asymmetry > 0) {
    interpretation <- sprintf(
      "Stronger co-movement during upturns (%.1f%% vs %.1f%%).",
      comovement_upturn * 100, comovement_downturn * 100
    )
  } else {
    interpretation <- sprintf(
      "Stronger co-movement during downturns (%.1f%% vs %.1f%%).",
      comovement_downturn * 100, comovement_upturn * 100
    )
  }
  
  if (!is.na(p_value) && p_value < 0.05) {
    interpretation <- paste(interpretation, "This difference is statistically significant.")
  }
  
  # Build result
  result <- list(
    overall = overall,
    upturn = comovement_upturn,
    downturn = comovement_downturn,
    asymmetry = asymmetry,
    n_upturn = n_upturn,
    n_downturn = n_downturn,
    p_value = p_value,
    reference = reference,
    interpretation = interpretation
  )
  
  class(result) <- "asymmetric_direction"
  result
}

#' @export
print.asymmetric_direction <- function(x, ...) {
  cat("\n\tAsymmetric Co-movement Analysis\n\n")
  cat("Reference series:", x$reference, "\n\n")
  cat("Overall co-movement:", sprintf("%.1f%%", x$overall * 100), "\n")
  cat("During upturns:", sprintf("%.1f%%", x$upturn * 100), 
      sprintf("(n = %d)\n", x$n_upturn))
  cat("During downturns:", sprintf("%.1f%%", x$downturn * 100),
      sprintf("(n = %d)\n", x$n_downturn))
  cat("Asymmetry:", sprintf("%.1f pp", x$asymmetry * 100), "\n")
  if (!is.na(x$p_value)) {
    cat("p-value:", format.pval(x$p_value, digits = 3), "\n")
  }
  cat("\nInterpretation:", x$interpretation, "\n")
  invisible(x)
}
