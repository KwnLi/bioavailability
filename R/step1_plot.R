#' Step 1 plot
#'
#' A GUI function.
#'
#' @param step1.output Output of `step1` function
#' @param error_threshold Desired error threshold line in plot
#'
#' @return A `ggplot2` plot object
#' @export
#'
step1_plot <- function(step1.output, error_threshold, sm.text = 14, lg.text = 16, plot.text = 4, guide.text = 14){
  frcAct <- step1.output$step1$frcAct[1]
  errortype <- ifelse(frcAct > 0, "False compliance", "False exceedance")

  step1results <- step1.output$step1 |>
    dplyr::mutate(aggregation = ifelse(n_incr==1, "discrete", paste("Incr. = ", n_incr)))

  step1results$aggregation <- factor(step1results$aggregation,
                                     levels = unique(step1results$aggregation[order(step1results$n_incr)]))

  ggplot2::ggplot(step1results,
         ggplot2::aes(x = n_tot, y = err_pb*100, color = aggregation)) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::xlab("Number of samples") +
    ggplot2::ylab(paste("Probability of", errortype, "error", sep = " ")) +
    ggplot2::geom_hline(yintercept = error_threshold, color = "black") +
    ggplot2::annotate("text", x=max(step1results$n_tot), y=error_threshold + 2,
             label="Error prob. objective",
             hjust=1, fontface="italic", size = plot.text) +
    ggplot2::ggtitle(paste(errortype, " error with more sampling, \nwhen true EPC ", step1.output$AsPb, " is ",
                  round(abs(frcAct*100),1), "% ",
                  ifelse(frcAct>0, "above", "below"),
                  " action level",
                  sep = ""),) +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Aggregation**")) +
    ggplot2::ylim(c(0, 100)) +
    egg::theme_article() +
    ggplot2::theme(legend.position = c(0.95,0.95),
          legend.justification = c(1,1),
          axis.text = ggplot2::element_text( size = sm.text ),
          axis.title = ggplot2::element_text( size = lg.text),
          legend.text = ggplot2::element_text(size = sm.text),
          legend.title = ggplot2::element_text(size = guide.text),
          plot.title = ggplot2::element_text(size = lg.text, face = "bold" )
    )
}
