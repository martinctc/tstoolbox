#' For a numeric vector of values, return its relation to the previous (lagged) value
#' 
#' @param x Numeric vector to pass through
#'@export
direction <- function(x){
  y = x - lag(x,1)
  ifelse(y > 0,
         "Positive",
         ifelse(y<0,
                "Negative",
                ifelse(y==0,
                       "Equal",NA)))
}

