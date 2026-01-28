#' Create a cross-correlation table
#'
#' Returns the values of the cross-correlation between two time series
#' variables as a tibble.
#'
#' @param df A data frame containing the time series variables.
#' @param x Unquoted name of the first numeric variable.
#' @param y Unquoted name of the second numeric variable.
#'
#' @return A tibble with two columns: `lag` (the lag value) and `x.corr`
#'   (the cross-correlation at that lag).
#'
#' @importFrom stats ccf
#' @importFrom tibble as_tibble
#'
#' @examples
#' df <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100)
#' )
#' xcf(df, x, y)
#'
#' @export
xcf <- function(df, x, y){
  df_x <- eval(substitute(x),df)
  df_y <- eval(substitute(y),df)
  ccf.object <- stats::ccf(df_x,df_y,plot = FALSE)

  output_table <-
    cbind(lag=ccf.object$lag, x.corr=ccf.object$acf) %>%
    tibble::as_tibble() %>%
    return()
}
