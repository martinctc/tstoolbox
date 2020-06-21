#' @title Create a score that measures the absolute fluctuation
#' for a numeric vector
#' @description
#' Returns the sum of absolute differences for each
#' value in a vector, between \emph{k} and \emph{k-1}.
#' 
#' @examples
#' p <- c(3, 5, 4, 2, 1, 7)
#' q <- c(4, 4, 3, 3, 4, 4)
#' sumlagdiff(p)
#' sumlagdiff(q)
#' 
#' @param x numeric
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' 
#' @export 
sumlagdiff <- function(x, na.rm = FALSE){
  raw_diff <- diff(x, lag = 1)
  abs_diff <- abs(raw_diff)
  sum_diff <- sum(abs_diff, na.rm = na.rm)
  sum_diff
}

