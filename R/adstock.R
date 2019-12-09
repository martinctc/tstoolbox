#' Calculate adstock (decay)
#'
#' @param x Numeric vector to be passed through.
#' @param rate Decay rate to be applied to `x`
#' @export
adstock <- function(x, rate = 0){
  x %>%
    stats::filter(filter = rate, method = "recursive") %>%
    as.numeric() %>% return()
}
