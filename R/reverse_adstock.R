#' Convert adstocked values back to original values
#' 
#'
#' @param x Numeric vector to be passed through
#' @param rate Adstock rate to be used (must be a positive value)
#' 
#' @export
reverse_adstock <- function(x, rate = 0){
  y <- x - rate * dplyr::lag(x)
  y[[1]] <- x[[1]] # replace first value
  return(y)
}

## Test of adstock reversal
# adstock(c(100, 200, 300, 150, 200), 0.2) %>%
#   reverse_adstock(rate = 0.2)