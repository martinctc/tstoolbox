#' Start-End Line
#'
#' Generates a linear vector between the start and end value of the input vector.
#'
#' @param x Numeric vector to pass through. Missing values other than the start
#'   and end value in the vector are ignored.
#'
#' @return A numeric vector of the same length as `x` with values linearly
#'   interpolated between the first and last values.
#'
#' @examples
#' stend_line(c(10, NA, NA, NA, 50))
#'
#' @export
stend_line <- function(x){
  seq(from = x[1],
      to = x[length(x)],
      length.out = length(x))
}
