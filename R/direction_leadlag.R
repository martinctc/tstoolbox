#' Detect lead-lag relationship in directional co-movement
#'
#' Determines whether one time series leads or lags another in terms of
#' directional changes, and identifies the optimal lag.
#'
#' @param x Numeric vector for the first time series.
#' @param y Numeric vector for the second time series (same length as `x`).
#' @param max_lag Maximum number of lags to test (in both directions).
#'   Defaults to 6.
#'
#' @return A list with class "direction_leadlag" containing:
#'   \item{optimal_lag}{The lag with highest co-movement. Negative means
#'     x leads y, positive means y leads x.}
#'   \item{max_comovement}{The co-movement proportion at optimal lag.}
#'   \item{lag_table}{A data frame with co-movement at each tested lag.}
#'   \item{interpretation}{Human-readable interpretation of the result.}
#'
#' @details
#' The function computes co-movement proportion for lags from `-max_lag`
#' to `+max_lag`:
#' \itemize{
#'   \item Negative lag: `x` at time t compared to `y` at time t+k (x leads)
#'   \item Positive lag: `x` at time t compared to `y` at time t-k (y leads)
#'   \item Zero lag: contemporaneous comparison
#' }
#'
#' @examples
#' # x leads y by 2 periods
#' set.seed(42)
#' x <- cumsum(rnorm(100))
#' y <- dplyr::lag(x, 2) + rnorm(100, sd = 0.3)
#' 
#' result <- direction_leadlag(x, y, max_lag = 5)
#' print(result)
#' plot(result)
#'
#' @export
direction_leadlag <- function(x, y, max_lag = 6) {
  
  # Input validation
  if (length(x) != length(y)) {
    stop("`x` and `y` must have the same length.")
  }
  if (max_lag < 1) {
    stop("`max_lag` must be at least 1.")
  }
  
  n <- length(x)
  lags <- seq(-max_lag, max_lag)
  
  # Calculate co-movement at each lag
  comovement <- sapply(lags, function(k) {
    if (k < 0) {
      # x leads: compare x[1:(n+k)] with y[(1-k):n]
      x_sub <- x[1:(n + k)]
      y_sub <- y[(1 - k):n]
    } else if (k > 0) {
      # y leads: compare x[(1+k):n] with y[1:(n-k)]
      x_sub <- x[(1 + k):n]
      y_sub <- y[1:(n - k)]
    } else {
      x_sub <- x
      y_sub <- y
    }
    
    dir_x <- direction(x_sub)
    dir_y <- direction(y_sub)
    
    valid <- !is.na(dir_x) & !is.na(dir_y)
    if (sum(valid) == 0) return(NA_real_)
    
    sum(dir_x[valid] == dir_y[valid]) / sum(valid)
  })
  
  # Find optimal lag
  optimal_idx <- which.max(comovement)
  optimal_lag <- lags[optimal_idx]
  max_comovement <- comovement[optimal_idx]
  
  # Create interpretation
  if (optimal_lag < 0) {
    interpretation <- sprintf(
      "x leads y by %d period(s) with %.1f%% co-movement",
      abs(optimal_lag), max_comovement * 100
    )
  } else if (optimal_lag > 0) {
    interpretation <- sprintf(
      "y leads x by %d period(s) with %.1f%% co-movement",
      optimal_lag, max_comovement * 100
    )
  } else {
    interpretation <- sprintf(
      "Contemporaneous relationship (no lead-lag) with %.1f%% co-movement",
      max_comovement * 100
    )
  }
  
  # Build result
  result <- list(
    optimal_lag = optimal_lag,
    max_comovement = max_comovement,
    lag_table = data.frame(
      lag = lags,
      comovement = comovement
    ),
    interpretation = interpretation
  )
  
  class(result) <- "direction_leadlag"
  result
}

#' @export
print.direction_leadlag <- function(x, ...) {
  cat("\n\tLead-Lag Direction Analysis\n\n")
  cat("Optimal lag:", x$optimal_lag, "\n")
  cat("Co-movement at optimal lag:", 
      sprintf("%.1f%%", x$max_comovement * 100), "\n\n")
  cat("Interpretation:", x$interpretation, "\n")
  invisible(x)
}

#' @export
#' @importFrom ggplot2 ggplot aes geom_col geom_vline scale_fill_gradient2
#'   labs theme_minimal
plot.direction_leadlag <- function(x, ...) {
  
  df <- x$lag_table
  
  # Avoid R CMD check notes
  lag <- comovement <- NULL
  
  ggplot2::ggplot(df, ggplot2::aes(x = lag, y = comovement, fill = comovement)) +
    ggplot2::geom_col() +
    ggplot2::geom_vline(xintercept = x$optimal_lag, linetype = "dashed", 
                        color = "red", linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dotted", 
                        color = "gray40") +
    ggplot2::scale_fill_gradient2(
      low = "#cc0000", mid = "#f0f0f0", high = "#339933",
      midpoint = 0.5, limits = c(0, 1)
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1),
                                labels = scales::percent_format()) +
    ggplot2::labs(
      title = "Lead-Lag Co-movement Analysis",
      subtitle = x$interpretation,
      x = "Lag (negative = x leads, positive = y leads)",
      y = "Co-movement proportion",
      fill = "Co-movement"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}
