#' @title Create a score that measures the absolute fluctuation
#'   for a numeric vector
#'
#' @description
#' Returns the sum of absolute differences for each
#' value in a vector, between \emph{k} and \emph{k-1}.
#'
#' @param x A numeric vector.
#' @param na.rm Logical. Should missing values (including `NaN`) be removed?
#'
#' @return A single numeric value representing the sum of absolute differences.
#'
#' @examples
#' p <- c(3, 5, 4, 2, 1, 7)
#' q <- c(4, 4, 3, 3, 4, 4)
#' sumlagdiff(p)
#' sumlagdiff(q)
#'
#' @export 
sumlagdiff <- function(x, na.rm = FALSE){
  raw_diff <- diff(x, lag = 1)
  abs_diff <- abs(raw_diff)
  sum_diff <- sum(abs_diff, na.rm = na.rm)
  sum_diff
}

