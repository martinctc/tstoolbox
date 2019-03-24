#' Start-End Line
#'
#' Generates a linear vector between the start and end value of the input vector.
#'
#' @param x Numeric vector to pass through. Missing values other than the start and end value
#' in the vector are ignored.
#' @export
stend_line <- function(x){
  seq(from = x[1],
      to = x[length(x)],
      length.out = length(x))
}
