# package checking and installation
# from: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

step1_plot <- function(step1.output){
  frcAct <- step1.output$step1$frcAct[1]
  errortype <- ifelse(frcAct > 0, "Type 1", "Type 2")
  
  step1results <- step1.output$step1 %>% 
    mutate(aggregation = ifelse(n_incr==1, "discrete", paste("Incr. = ", n_incr)))
  
  step1results$aggregation <- factor(step1results$aggregation,
                                     levels = unique(step1results$aggregation[order(step1results$n_incr)]))
  
  ggplot(step1results,
         aes(x = n_tot, y = err_pb*100, color = aggregation)) + 
    geom_line() + geom_point() + 
    xlab("Num. samples analyzed (total concentration)") + 
    ylab(paste("Probability of", errortype, "error", sep = " ")) +
    geom_hline(yintercept = ifelse(errortype == "Type 1", 5, 20), color = "red") + 
    ggtitle(paste(errortype, " error with more sampling, when true \nbioavailable ", step1.output$AsPb, " is ",
                  round(abs(frcAct*100),1), "% ", 
                  ifelse(frcAct>0, "above", "below"), 
                  " action level",
                  sep = ""),) + 
    ylim(c(0,100)) +
    theme_article() +
    theme(legend.position = c(0.95,0.95),
          legend.justification = c(1,1),
          axis.text = element_text( size = 14 ),
          axis.title = element_text( size = 16, face = "bold" ),
          legend.text=element_text(size = 14),
          plot.title = element_text(size = 20)
    )
}


step2_plot <- function(step2.output){
  errortype <- ifelse(median(step2.output$step2$frcAct) > 0, "Type 1", "Type 2")
  
  ggplot(step2.output$step2, aes(x = 100*frcAct, y = err_pb*100)) + 
    geom_line() + geom_point() + 
    ylab(paste("Probability of", errortype, "error", sep = " ")) +
    xlab(paste("% ", ifelse(errortype == "Type 1", "above", "below"), " action level", sep = "")) +
    geom_hline(yintercept = ifelse(errortype == "Type 1", 5, 20), color = "red") + 
    ggtitle(
      paste(errortype, " error with ", ifelse(errortype=="Type 1", "increasing", "decreasing"),
            "\nbioavailable ", step2.output$AsPb,
            ifelse(errortype=="Type 1", " above", " below"), " the action level",
                  sep = ""),) + 
    ylim(c(0,100)) +
    theme_article() +
    theme(axis.text = element_text( size = 14 ),
          axis.title = element_text( size = 16, face = "bold" ),
          legend.text=element_text(size = 14),
          plot.title = element_text(size = 20)
          )
}

step4_plot <- function(step4.output){
  
  measured.EPC <- step4.output$step3$meas.ba
  action.level <- step4.output$step3$actLvl.adj
  
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
  ) %>% bind_rows(.id = "Simulation")
  
  hatchresults <- step4results %>%
    filter((Simulation == "Accuracy" & ba_DU <= measured.EPC)|
             (Simulation == "Precision" & ba_DU > action.level))
  
  ggplot(step4results, aes(ba_DU)) +
    geom_histogram(breaks = sort(custombreaks), fill = "wheat", color = "black") + 
    geom_histogram_pattern(data = hatchresults,
                           mapping = aes(ba_DU),
                           breaks = sort(custombreaks), 
                           fill = NA,
                           color = NA,
                           pattern_fill = "black",
                           pattern_angle = 45,
                           pattern_density = 0.1,
                           pattern_spacing = 0.01,) + 
    facet_wrap(~Simulation) + 
    theme_article() +
    geom_vline(data = data.frame(threshold = c("Measured EPC", "Action level"),
                                 value = c(measured.EPC, action.level)),
      mapping = aes(xintercept = value, linetype = threshold, color = threshold), 
               size = 0.8, key_glyph = "path") +
    scale_linetype_manual(values = c(1, 2)) +
    scale_color_manual(values = c("red", "blue")) +
    labs(linetype = "", color = "") +
    xlab("Model-estimated measured EPC (mg kg-1)\nacross 5,000 model iterations") +
    theme(axis.text = element_text( size = 14 ),
          axis.title = element_text( size = 16, face = "bold" ),
          strip.text = element_text(size = 20),
          legend.text=element_text(size = 14),
          legend.position = "top")
}
