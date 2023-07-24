#' Title
#'
#' @param datTrans An object of type transaction
#'
#' @return to be written
#' @export
#'
#' @examples
#' to be written
#'
plotFreqItems <- function(datFreq, topN){

  require(data.table)
  n_items_to_plot <- max(min(topN, nrow(datFreq$freqTable)), 1)

  datTAB <- data.table(datFreq$freqTable)
  datTAB[, order := 1:.N]
  datTAB[, label := if(order>n_items_to_plot){"(other)"}else{item}, by=c("order")]

  datTAB[, label:= stringr::str_wrap(label, width = 14)]

  datTAB_toPlot <- datTAB[label != "(other)"]
  datTAB_toPlot[, label:=factor(label, levels=label)]


  output <- ggplot2::ggplot(data=datTAB_toPlot, ggplot2::aes(x=label, y=freq)) +
    ggplot2::geom_bar(stat="identity", fill="dodgerblue4", alpha=0.8) +
    ggplot2::scale_fill_manual(values=ggplot2::alpha(c("dodgerblue4"), 0.8)) +
    ggplot2::scale_y_continuous(limits = c(0, 0.8), labels = scales::percent) +
    ggplot2::geom_text(ggplot2::aes(x=label, y=freq, label=cases),
                       size=6,
                       vjust = -0.5,
                       position = ggplot2::position_dodge(width=1)) +
    ggplot2::xlab("\nItems") +
    ggplot2::ylab("Frequency\n") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title=ggplot2::element_text(size=24),
                   legend.position="none",
                   axis.text=ggplot2::element_text(size=18),
                   axis.text.x=ggplot2::element_text(angle = 45, vjust = 1, hjust=1),
                   axis.title.y=ggplot2::element_text(size=22),
                   axis.title.x=ggplot2::element_text(size=22))
  return(output)
}

