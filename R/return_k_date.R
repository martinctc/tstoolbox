#' Return the k-th most recent or oldest date / datetime from a vector
#'
#' @param x A vector of date-time values.
#' @param k Integer specifying the k-th value to return from the vector.
#' @param decreasing Logical. If `TRUE` (default), returns the k-th most recent
#'   date. If `FALSE`, returns the k-th oldest date.
#'
#' @return A single datetime value representing the k-th date.
#'
#' @import lubridate
#' @importFrom magrittr %>%
#' @importFrom Rfast nth
#'
#' @examples
#' library(lubridate)
#' dates <- c(ymd("2018-01-01"), ymd("2016-01-31"), ymd("2017-01-31"))
#' return_k_date(dates, k = 2)
#'
#' @export
return_k_date <- function(x, k, decreasing = TRUE){
  x <- lubridate::as_datetime(x)

  Rfast::nth(x, k, descending = decreasing) %>%
    lubridate::as_datetime()
}
