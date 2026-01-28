#' Calculate percentage change of a vector relative to a lag k
#'
#' @param x A numeric vector to be passed through.
#' @param lag The number of lags used, defaults to 1.
#'
#' @return A numeric vector of the same length as `x` containing the
#'   percentage change relative to the lagged value. The first `lag`
#'   elements will be `NA`.
#'
#' @examples
#' pc_change(c(100, 110, 121, 100))
#' pc_change(c(100, 110, 121, 100), lag = 2)
#'
#' @export
pc_change <-function(x,lag=1){
  base <- lag(x,lag) # base to be divided by
  c(rep(NA,lag),diff(x,lag))/base
}