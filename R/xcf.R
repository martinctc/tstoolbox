#' Create a cross-correlation table
#'
#' Returns the values of the cross-correlation correlation
#'
#' @param x Numeric vector to pass through
#' @param y Numeric vector to pass through
#' @export
xcf <- function(df, x, y){
  df_x <- eval(substitute(x),df)
  df_y <- eval(substitute(y),df)
  ccf.object <- ccf(df_x,df_y,plot = FALSE)

  output_table <-
    cbind(lag=ccf.object$lag, x.corr=ccf.object$acf) %>%
    as_tibble() %>%
    return()
}
