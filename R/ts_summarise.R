#' Group-summarise a time series
#'
#' @param x Data frame to be passed through
#' @param grouping Method of grouping / rounding used for dates. See `lubridate::floor_date()`
#' @param date_var Character string specifying variable name with the date variable
#' @param fun Character string specifying function for summarising. Defaults to sum
#' @param ... Additional arguments
#'
#' @export
ts_summarise <- function(x, grouping="year", date_var = "Date", fun = "sum",  ...){
  x %>%
    group_by(!!sym(grouping) := floor_date(!!sym(date_var), grouping)) %>%
    summarise_at(vars(-!!sym(date_var)),~do.call(fun, list(., ...)))
}
