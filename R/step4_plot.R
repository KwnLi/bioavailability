#' Step 4 plot
#'
#' @param step4.output output of `step4` function
#'
#' @return A list containing the output plot, an accuracy text string, and a
#'   precision text string
#' @export
#'
step4_plot <- function(step4.output){

  measured.EPC <- step4.output$step3$meas.ba
  action.level <- step4.output$step3$actLvl.adj

  error.type <- ifelse(measured.EPC<action.level, "False compliance", "False exceedance")

  total.range <- range(c(step4.output$accuracy.sim$DU_sim$ba_DU, step4.output$precision.sim$DU_sim$ba_DU))

  EPC.AL.bin.separation <- round(30*abs(measured.EPC-action.level)/diff(total.range),0) # number of histogram bins separating EPC and AL
  bin.size <- abs(measured.EPC-action.level)/EPC.AL.bin.separation

  # browser()

  # create custom bins
  custombreaks <- action.level

  # add custom bins above AL
  while(max(custombreaks) <= max(total.range)+bin.size){
    custombreaks <- append(custombreaks, max(custombreaks)+bin.size)
  }
  # add custom bins below AL
  while(min(custombreaks) >= min(total.range)-bin.size){
    custombreaks <- append(custombreaks, min(custombreaks)-bin.size)
  }

  step4results <- list(
    Accuracy = step4.output$accuracy.sim$DU_sim,
    Precision = step4.output$precision.sim$DU_sim
  ) |> dplyr::bind_rows(.id = "Simulation")

  hatchresults <- step4results |>
    dplyr::filter(if(error.type == "False compliance"){
      (Simulation == "Accuracy" & ba_DU <= measured.EPC)|(Simulation == "Precision" & ba_DU > action.level)
    }else{
      (Simulation == "Accuracy" & ba_DU > measured.EPC)|(Simulation == "Precision" & ba_DU <= action.level)
    })

  accuracy_ct <- if(error.type == "False compliance"){
    sum(step4.output$accuracy.sim$DU_sim$ba_DU < measured.EPC)
  }else{
    sum(step4.output$accuracy.sim$DU_sim$ba_DU > measured.EPC)
  }

  precision_ct <- if(error.type == "False compliance"){
    sum(step4.output$precision.sim$DU_sim$ba_DU > action.level)
  }else{
    sum(step4.output$precision.sim$DU_sim$ba_DU < action.level)
  }

  sim_ct <- nrow(step4.output$accuracy.sim$DU_sim)

  outplot <- ggplot2::ggplot(step4results, ggplot2::aes(ba_DU)) +
    ggplot2::geom_histogram(breaks = sort(custombreaks), fill = "wheat", color = "black") +
    ggpattern::geom_histogram_pattern(data = hatchresults,
                           mapping = ggplot2::aes(ba_DU),
                           breaks = sort(custombreaks),
                           fill = NA,
                           color = NA,
                           pattern_fill = "black",
                           pattern_angle = 45,
                           pattern_density = 0.1,
                           pattern_spacing = 0.01,) +
    ggplot2::facet_wrap(~Simulation) +
    egg::theme_article() +
    ggplot2::geom_vline(data = data.frame(threshold = c("Measured EPC", "Action level"),
                                 value = c(measured.EPC, action.level)),
               mapping = ggplot2::aes(xintercept = value, linetype = threshold, color = threshold),
               linewidth = 0.8, key_glyph = "path") +
    ggplot2::scale_linetype_manual(values = c(1, 2)) +
    ggplot2::scale_color_manual(values = c("red", "blue")) +
    ggplot2::labs(linetype = "", color = "") +
    ggplot2::xlab(paste0("Model-estimated measured EPC (mg kg-1)\nacross ",
                sim_ct, " model iterations")) +
    ggplot2::theme(axis.text = ggplot2::element_text( size = 14 ),
          axis.title = ggplot2::element_text( size = 16, face = "bold" ),
          strip.text = ggplot2::element_text(size = 20),
          legend.text= ggplot2::element_text(size = 14),
          legend.position = "top")

  accuracyText <- paste0(
    "<b>Accuracy:</b> ", round(100*accuracy_ct/sim_ct, 1),
    "% of the simulated measured bioavailability-adjusted EPCs (i.e., ",
    accuracy_ct, " out of ", sim_ct, " simulations) are ",
    ifelse(error.type == "False compliance", "less than", "greater than"),
    " the observed EPC assuming the DU's true EPC equals the AL.",
    " Therefore, there is a <=",
    round(100*accuracy_ct/sim_ct, 1), "% probability that the measured EPC observed from sampling came from a decision unit with a true EPC ",
    ifelse(error.type == "False compliance", ">=", "<="), " the AL."
  )

  precisionText <- paste0(
    "<b>Precision:</b> ", round(100*precision_ct/sim_ct, 1),
    "% of the simulated measured bioavailability-adjusted EPCs (i.e., ",
    precision_ct, " out of ", sim_ct, " simulations) are ",
    ifelse(error.type == "False compliance", "greater than", "less than"),
    " the AL assuming the measured EPC (from sampling) is the mean measured EPC if you were to repeat sampling ",
    sim_ct, " times. ",
    "Therefore, there is a ", round(100*precision_ct/sim_ct, 1),
    "% probability that resampling, using the same sampling plan, would lead to a different assessment of the EPC relative to the AL (in this case that the EPC is ",
    ifelse(error.type == "False compliance", ">", "<"), " the AL). ",
    "This assumes that the measured EPC is the mean EPC that would be observed if sampling were repeated ",
    sim_ct, " times."
  )

  return(list(outplot = outplot, accuracyText = accuracyText, precisionText = precisionText))
}
