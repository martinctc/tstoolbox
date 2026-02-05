#' Return the direction of change relative to the previous value
#'
#' For a numeric vector of values, return its relation to the previous
#' (lagged) value as "Positive", "Negative", or "Equal".
#'
#' @param x Numeric vector to pass through.
#'
#' @return A character vector of the same length as `x`, with values
#'   "Positive", "Negative", "Equal", or `NA`.
#'
#' @importFrom dplyr lag
#'
#' @examples
#' direction(c(1, 3, 2, 5, 5))
#'
#' @export
direction <- function(x){
  y = x - dplyr::lag(x,1)
  ifelse(y > 0,
         "Positive",
         ifelse(y<0,
                "Negative",
                ifelse(y==0,
                       "Equal",NA)))
}

