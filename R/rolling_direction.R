#' Calculate rolling co-movement proportion between two time series
#'
#' Computes the proportion of periods where two time series move in the same
#' direction over a rolling window. This helps identify how co-movement
#' changes over time.
#'
#' @param x Numeric vector for the first time series.
#' @param y Numeric vector for the second time series (same length as `x`).
#' @param window Integer specifying the rolling window size. Defaults to 12.
#' @param align Character string specifying window alignment: "right" (default),
#'   "center", or "left".
#' @param min_obs Minimum number of non-NA observations required in each window.
#'   Defaults to `window / 2`.
#'
#' @return A numeric vector of the same length as `x` containing the rolling
#'   co-movement proportion (0 to 1). Values at the start will be `NA` depending
#'   on alignment and window size.
#'
#' @details
#' For each window, the function:
#' 1. Computes the direction of change for both series

#' 2. Counts how many periods have matching directions
#' 3. Returns the proportion of matches
#'
#' A value of 1 means perfect co-movement (always move together),
#' 0.5 suggests random/no relationship, and values near 0 indicate
#' inverse movement.
#'
#' @examples
#' # Simulated co-moving series
#' set.seed(123)
#' x <- cumsum(rnorm(100))
#' y <- x + rnorm(100, sd = 0.5)  
#' rolling_direction(x, y, window = 12)
#'
#' # See how co-movement changes over time
#' library(ggplot2)
#' df <- data.frame(
#'   t = 1:100,
#'   comovement = rolling_direction(x, y, window = 12)
#' )
#' ggplot(df, aes(t, comovement)) + geom_line() + ylim(0, 1)
#'
#' @export
rolling_direction <- function(x, y, window = 12, align = "right", min_obs = NULL) {

  # Input validation

  if (length(x) != length(y)) {
    stop("`x` and `y` must have the same length.")
  }
  if (window < 2) {
    stop("`window` must be at least 2.")
  }
  if (!align %in% c("right", "center", "left")) {
    stop("`align` must be one of 'right', 'center', or 'left'.")
  }
  
  if (is.null(min_obs)) {
    min_obs <- ceiling(window / 2)
  }
  
  n <- length(x)
  

  # Get directions
  dir_x <- direction(x)
  dir_y <- direction(y)
  
  # Check for matching directions
  match <- dir_x == dir_y
  
  # Calculate rolling proportion
  result <- rep(NA_real_, n)
  
  for (i in seq_len(n)) {
    if (align == "right") {
      start_idx <- i - window + 1
      end_idx <- i
    } else if (align == "left") {
      start_idx <- i
      end_idx <- i + window - 1
    } else {
      half_win <- floor(window / 2)
      start_idx <- i - half_win
      end_idx <- i + (window - half_win - 1)
    }
    
    if (start_idx < 1 || end_idx > n) {
      next
    }
    
    window_match <- match[start_idx:end_idx]
    valid_obs <- sum(!is.na(window_match))
    
    if (valid_obs >= min_obs) {
      result[i] <- sum(window_match, na.rm = TRUE) / valid_obs
    }
  }
  
  result
}
