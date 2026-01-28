#' Calculate adstock (decay)
#'
#' Applies an adstock (decay) transformation to a numeric vector using
#' a recursive filter.
#'
#' @param x Numeric vector to be passed through.
#' @param rate Decay rate to be applied to `x`. Must be between 0 and 1.
#'
#' @return A numeric vector of the same length as `x` with the adstock
#'   transformation applied.
#'
#' @examples
#' # Apply 20% decay rate
#' adstock(c(100, 200, 300, 150, 200), rate = 0.2)
#'
#' @export
adstock <- function(x, rate = 0){
  x %>%
    stats::filter(filter = rate, method = "recursive") %>%
    as.numeric() %>% return()
}
