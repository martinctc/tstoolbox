#' Return the k-th most recent or oldest date / datetime from a vector
#'
#'
#' @param x A vector of date-time
#' @param k Integer specifying the k-th value to return from the vector
#' @param decreasing Logical. Specifies whether to return most recent or oldest
#'
#' @import lubridate
#' @importFrom magrittr %>%
#' @importFrom Rfast nth
#' @examples
#' library(lubridate)
#' dates <- c(ymd("2018-01-01"), ymd("2016-01-31"), ymd("2017-01-31"))
#' return_k_date(dates, k = 2)
#' @export
return_k_date <- function(x, k, decreasing = TRUE){
  x <- lubridate::as_datetime(x)

  Rfast::nth(x, k, descending = decreasing) %>%
    lubridate::as_datetime()
}
