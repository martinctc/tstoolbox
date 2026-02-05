#' Analyse co-movement between two numeric variables
#'
#' This returns the total number and proportion of pairwise co-movement in two
#' time series variables. An explanatory note is printed as a message in the console.
#'
#' @param x A data frame containing the time series variables.
#' @param var1 Unquoted name of the first numeric variable.
#' @param var2 Unquoted name of the second numeric variable.
#'
#' @return A tibble with three columns: `n` (number of matching directions),
#'   `base` (total number of observations), and `prop` (proportion of matches).
#'
#' @importFrom rlang enquo `:=`
#' @importFrom dplyr vars mutate_at mutate summarise
#' @importFrom glue glue
#' @importFrom tidyr drop_na
#'
#' @examples
#' df <- data.frame(
#'   series1 = c(1, 3, 2, 5, 4),
#'   series2 = c(2, 4, 3, 6, 5)
#' )
#' analyse_direction(df, series1, series2)
#'
#' @export
analyse_direction <- function(x,var1,var2){
  var1 <- rlang::enquo(var1)
  var2 <- rlang::enquo(var2)

  x %>%
    dplyr::mutate_at(dplyr::vars(!!var1,!!var2),~direction(.)) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(match:=(!!var1)==(!!var2)) %>%
    dplyr::summarise(n=sum(match),
              base=length(match),
              prop=sum(match)/length(match)) -> result

  prop <- paste0(round((result$prop)*100),"%")

  message(glue::glue('There are {result$n} out of {result$base} instance(s) ({prop}) where values move in the same direction.'))
  return(result)
}
