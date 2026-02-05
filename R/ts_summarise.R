#' Group-summarise a time series
#'
#' Aggregates a time series data frame by a specified time grouping (e.g., year,
#' month, week) using a summary function.
#'
#' @param x Data frame to be passed through.
#' @param grouping Method of grouping / rounding used for dates. See
#'   [lubridate::floor_date()] for valid values (e.g., "year", "month", "week").
#' @param date_var Character string specifying variable name with the date variable.
#' @param fun Character string specifying function for summarising. Defaults to "sum".
#' @param ... Additional arguments passed to the summary function.
#'
#' @return A summarised data frame grouped by the specified time interval.
#'
#' @importFrom rlang sym `:=`
#' @importFrom dplyr group_by summarise_at vars
#' @importFrom lubridate floor_date
#'
#' @examples
#' df <- data.frame(
#'   Date = as.Date(c("2020-01-15", "2020-01-20", "2020-02-10", "2020-02-25")),
#'   value = c(10, 20, 30, 40)
#' )
#' ts_summarise(df, grouping = "month", date_var = "Date")
#'
#' @export
ts_summarise <- function(x, grouping="year", date_var = "Date", fun = "sum",  ...){
  x %>%
    dplyr::group_by(!!rlang::sym(grouping) := lubridate::floor_date(!!rlang::sym(date_var), grouping)) %>%
    dplyr::summarise_at(dplyr::vars(-!!rlang::sym(date_var)),~do.call(fun, list(., ...)))
}
