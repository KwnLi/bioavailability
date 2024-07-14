#' Step 2 plot
#'
#' A GUI function
#'
#' @param step2.output Output of `step2` function
#' @param error_threshold Desired error threshold
#'
#' @return a `ggplot2` plot object
#' @export
#'
step2_plot <- function(step2.output, error_threshold, sm.text = 14, lg.text = 16){
  errortype <- ifelse(median(step2.output$step2$frcAct) > 0, "False compliance", "False exceedance")

  ggplot2::ggplot(step2.output$step2, ggplot2::aes(x = 100*frcAct, y = err_pb*100)) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::ylab(paste("Probability of", errortype, "error", sep = " ")) +
    ggplot2::xlab(paste0("% assumed true EPC ", ifelse(errortype == "False compliance", "above", "below"), " the action level")) +
    ggplot2::geom_hline(yintercept = error_threshold, color = "red") +
    ggplot2::ggtitle(
      paste(errortype, " error with ", ifelse(errortype=="False compliance", "increasing", "decreasing"),
            "\nbioavailable ", step2.output$AsPb,
            ifelse(errortype=="False compliance", " above", " below"), " the action level",
            sep = ""),) +
    ggplot2::ylim(c(0,100)) +
    egg::theme_article() +
    ggplot2::theme(axis.text = ggplot2::element_text( size = sm.text ),
          axis.title = ggplot2::element_text( size = lg.text),
          legend.text= ggplot2::element_text(size = sm.text),
          plot.title = ggplot2::element_text(size = lg.text, face = "bold" )
    )
}
