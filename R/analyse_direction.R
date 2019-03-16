#' Analyse co-movement between two numeric variables.
#'
#' An explanatory note is printed as a message in the console.
#'
#'@export
analyse_direction <- function(x,var1,var2){
  var1 <- enquo(var1)
  var2 <- enquo(var2)

  x %>%
    dplyr::mutate_at(vars(!!var1,!!var2),~direction(.)) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(match:=(!!var1)==(!!var2)) %>%
    dplyr::summarise(n=sum(match),
              base=length(match),
              prop=sum(match)/length(match)) -> result

  prop <- paste0(round((result$prop)*100),"%")

  message(glue::glue('There are {result$n} out of {result$base} instance(s) ({prop}) where values move in the same direction.'))
  return(result)
}
