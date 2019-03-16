#' Max-Min Line
#'
#' Generates a linear vector between the maximum and minimum value of the input vector.
#'
#' @export
maxmin_line <- function(x){
  seq(from = x[1],
      to = x[length(x)],
      length.out = length(x))
}
