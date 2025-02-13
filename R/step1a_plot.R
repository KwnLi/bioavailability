#' Step1a plot
#'
#' A GUI function.
#'
#' @param step1a.output Output of step1a.
#'
#' @return A `ggplot2` plot object
#' @export
#'
step1a_plot <- function(step1a.output, sm.text = 14, lg.text = 16, hatch.spacing = 0.01){
  measured.EPC <- step1a.output$err_pb$tru_ba
  action.level <- step1a.output$err_pb$actLvl.adj

  total.range <- range(step1a.output$DU_sim$ba_DU)

  EPC.AL.bin.separation <- round(30*abs(measured.EPC-action.level)/diff(total.range),0) # number of histogram bins separating EPC and AL
  bin.size <- ifelse(EPC.AL.bin.separation==0,
                     diff(total.range)/30,
                     abs(measured.EPC-action.level)/EPC.AL.bin.separation)

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

  hatchresults <- step1a.output$DU_sim |>
    dplyr::filter(if(measured.EPC < action.level){
      ba_DU <= action.level
    }else{
      ba_DU > action.level
    })

  correct_ct <- nrow(hatchresults)
  sim_ct <- nrow(step1a.output$DU_sim)

  outplot <- ggplot2::ggplot(step1a.output$DU_sim, ggplot2::aes(ba_DU)) +
    ggplot2::geom_histogram(breaks = sort(custombreaks), fill = "wheat", color = "black") +
    ggpattern::geom_histogram_pattern(data = hatchresults,
                           mapping = ggplot2::aes(ba_DU),
                           breaks = sort(custombreaks),
                           fill = NA,
                           color = NA,
                           pattern_fill = "black",
                           pattern_angle = 45,
                           pattern_density = 0.1,
                           pattern_spacing = hatch.spacing,) +
    egg::theme_article() +
    ggplot2::geom_vline(data = data.frame(threshold = c("Assumed true EPC", "Action level"),
                                 value = c(measured.EPC, action.level)),
               mapping = ggplot2::aes(xintercept = value, linetype = threshold, color = threshold),
               linewidth = 0.8, key_glyph = "path") +
    ggplot2::scale_linetype_manual(values = c(1, 2)) +
    ggplot2::scale_color_manual(values = c("red", "blue")) +
    ggplot2::labs(linetype = "", color = "") +
    ggplot2::xlab(paste0("Model-estimated measured EPC (mg kg-1)\nacross ",
                sim_ct, " model iterations")) +
    ggplot2::ggtitle(
      ifelse(measured.EPC < action.level,
             "False exceedance decision error probability",
             "False compliance decision error probability"
      )
    ) +
    ggplot2::theme(axis.text = ggplot2::element_text( size = sm.text ),
          axis.title = ggplot2::element_text( size = lg.text),
          strip.text = ggplot2::element_text(size = lg.text),
          legend.text= ggplot2::element_text(size = sm.text),
          plot.title = ggplot2::element_text(size = lg.text, face = "bold" ),
          legend.position = "top")

  outputText <- paste0(
    ifelse(measured.EPC < action.level,
           "<b>False exceedance decision error probability = ",
           "<b>False compliance decision error probability = "
    ),
    round(100-(100*correct_ct/sim_ct), 1), "% (",
    sim_ct-correct_ct, " out of ", sim_ct,
    " simulations):</b> ",
    "When the assumed true EPC is ",
    abs(round(100*step1a.output$err_pb$frcAct, 1)),
    "% ", ifelse(measured.EPC < action.level, "less than", "greater than"),
    " the action level, ",
    round(100*correct_ct/sim_ct, 1),
    "% of the simulated measured EPCs (i.e., ",
    correct_ct, " out of ", sim_ct, " simulations) are ",
    ifelse(measured.EPC < action.level, "less than", "greater than"),
    " the action level, leading to the correct assessment that the EPC is ",
    ifelse(measured.EPC < action.level, "below", "above"),
    " the action level; ",
    round(100-(100*correct_ct/sim_ct), 1), "% of the simulated measured EPCs (",
    sim_ct-correct_ct, " out of ", sim_ct,
    " simulations) lead to the incorrect assessment that the EPC is ",
    ifelse(measured.EPC < action.level, "above", "below"),
    " the action level, resulting in a ",
    ifelse(measured.EPC < action.level,
           "false exceedance decision error.",
           "false compliance decision error."
    )
  )

  return(list(outplot, outputText))

}
