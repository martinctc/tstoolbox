#' Create a pretty cross-correlation plot
#'
#' Generates a cross-correlation plot as a ggplot object with color-coded
#' bars indicating positive and negative correlations.
#'
#' @param df A data frame containing the time series variables.
#' @param x Unquoted name of the first numeric variable.
#' @param y Unquoted name of the second numeric variable.
#' @param title Character string specifying the plot title.
#'   Defaults to "Cross Correlation".
#'
#' @return A ggplot object displaying the cross-correlation plot.
#'
#' @importFrom stats ccf
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual ylab
#'   scale_y_continuous theme element_text ggtitle
#' @importFrom ggthemes theme_economist
#'
#' @examples
#' df <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100)
#' )
#' plot_xcf(df, x, y)
#'
#' @export
plot_xcf <- function(df, x, y, title="Cross Correlation"){

  df_x <- eval(substitute(x),df)
  df_y <- eval(substitute(y),df)
  ccf.object <- stats::ccf(df_x,df_y,plot=FALSE)

  # Avoid R CMD check notes for global variable bindings
  lag <- x.corr <- cat <- NULL

  output_table <-
    cbind(lag=ccf.object$lag, x.corr=ccf.object$acf) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(cat=ifelse(x.corr>0,"green","red"))

  output_table %>%
    ggplot2::ggplot(ggplot2::aes(x=lag,y=x.corr)) +
    ggplot2::geom_bar(stat="identity",ggplot2::aes(fill=cat))+
    ggplot2::scale_fill_manual(values=c("#339933","#cc0000"))+
    ggplot2::ylab("Cross correlation")+
    ggplot2::scale_y_continuous(limits=c(-1, 1))+
    ggthemes::theme_economist()+
    ggplot2::theme(legend.position = "none", plot.title=ggplot2::element_text(size=10))+
    ggplot2::ggtitle(title)
}
