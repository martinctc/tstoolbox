#' Create a pretty cross-correlation plot
#'
#' Returns a ggplot object
#'
#' @export
plot_xcf <- function(df, x, y, title="Cross Correlation"){
  df_x <- eval(substitute(x),df)
  df_y <- eval(substitute(y),df)
  ccf.object <- ccf(df_x,df_y,plot=FALSE)

  output_table <-
    cbind(lag=ccf.object$lag, x.corr=ccf.object$acf) %>%
    as_tibble() %>%
    mutate(cat=ifelse(x.corr>0,"green","red"))

  output_table %>%
    ggplot(aes(x=lag,y=x.corr)) +
    geom_bar(stat="identity",aes(fill=cat))+
    scale_fill_manual(values=c("#339933","#cc0000"))+
    ylab("Cross correlation")+
    scale_y_continuous(limits=c(-0.5,0.5))+
    ggthemes::theme_economist()+
    theme(legend.position = "none", plot.title=element_text(size=10))+
    ggtitle(title)
  # ggsave(paste(title,".svg"),plot=p,height=2.7,width=4,units="in")
}
