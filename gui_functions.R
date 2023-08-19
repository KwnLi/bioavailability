# GUI functions


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

step2_text <- function(step2_t1, step2_t2){
  type1_df <- step2_t1$step2 %>%
    mutate(from.05 = err_pb - .05) %>% # find difference from 0.05 threshold
    mutate(dif.neg = from.05 <= 0) %>% # sign of difference
    mutate(lag.neg = lag(dif.neg), lead.neg = lead(dif.neg))
  
  type2_df <- step2_t2$step2 %>%
    mutate(from.20 = err_pb - .20) %>% # find difference from 0.20 threshold
    mutate(dif.neg = from.20 <= 0) %>% # sign of difference
    mutate(lag.neg = lag(dif.neg), lead.neg = lead(dif.neg))

  # sort by absolute difference from threshold and find 2 closest
  nearest.type1 <- type1_df %>% filter(from.05 < 0) %>% filter(from.05 == max(from.05)) %>%
    bind_rows(type1_df %>% filter(from.05 > 0) %>% filter(from.05 == min(from.05)))
  nearest.type2 <- type2_df %>% filter(from.20 < 0) %>% filter(from.20 == max(from.20)) %>%
    bind_rows(type2_df %>% filter(from.20 > 0) %>% filter(from.20 == min(from.20)))
  
  # function to find interpolated value given two points (err_pb is the y)
  interp <- function(x, y, middle.point.y){
    slope = (y[2]-y[1])/(x[2]-x[1])
    return(x[1] + (middle.point.y - y[1])/slope)
  }
  
  # find where error crosses threshold (type 1)
  if(nrow(nearest.type1)!=2){ # test if both same logical (==1)
    message.type1 <- " the sampling protocol evaluated does not achieve target false compliance (Type 1) decision error probabilities across the range in the DU's true EPC assessed."
  }else{
    frcAct.t1 <- interp(x=nearest.type1$frcAct, y=nearest.type1$err_pb, middle.point.y=0.05)
    tru_ba.t1 <- interp(x=nearest.type1$tru_ba, y=nearest.type1$err_pb, middle.point.y=0.05)
    
    message.type1 <- paste0(", so long as the DU's true bioavailability-adjusted EPC is greater than ",
                           round(tru_ba.t1, 0), " mg bioavailable Pb (i.e., greater than ",
                           round(100*frcAct.t1, 1), "% above the ",
                           nearest.type1$actLvl.adj[1], " ppm AL), the sampling protocol evaluated will achieve satisfactory decision error probability goals for false compliance.")
  }
  
  # find where error crosses threshold (type 2)
  if(nrow(nearest.type2)!=2){ # test if both same logical (==1)
    message.type2 <- " the sampling protocol evaluated does not achieve target false exceedance (Type 2) decision error probabilities across the range in the DU's true EPC assessed."
  }else{
    frcAct.t2 <- interp(x=nearest.type2$frcAct, y=nearest.type2$err_pb, middle.point.y=0.20)
    tru_ba.t2 <- interp(x=nearest.type2$tru_ba, y=nearest.type2$err_pb, middle.point.y=0.20)
    
    message.type2 <- paste0(", so long as the DU's true bioavailability-adjusted EPC is less than ",
                           round(tru_ba.t2, 0), " mg bioavailable Pb (i.e., less than ",
                           round(100*abs(frcAct.t2), 1), "% below the ",
                           nearest.type2$actLvl.adj[1], " ppm AL), the sampling protocol evaluated will achieve satisfactory decision error probability goals for false exceedance")
  }

  return(list(
    type1 = paste0("<b>Type 1 error:</b> the simulation estimates that", message.type1),
    type2 = paste0("<b>Type 2 error:</b> the simulation estimates that", message.type2)
  ))
}

step4_plot <- function(step4.output){
  
  measured.EPC <- step4.output$step3$meas.ba
  action.level <- step4.output$step3$actLvl.adj
  
  error.type <- ifelse(measured.EPC<action.level, "Type 1", "Type 2")
  
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
    filter(if(error.type == "Type 1"){
        (Simulation == "Accuracy" & ba_DU <= measured.EPC)|(Simulation == "Precision" & ba_DU > action.level)
      }else{
        (Simulation == "Accuracy" & ba_DU > measured.EPC)|(Simulation == "Precision" & ba_DU <= action.level)
      })
  
  accuracy_ct <- if(error.type == "Type 1"){
    sum(step4.output$accuracy.sim$DU_sim$ba_DU < measured.EPC)
  }else{
    sum(step4.output$accuracy.sim$DU_sim$ba_DU > measured.EPC)
  }
  
  precision_ct <- if(error.type == "Type 1"){
    sum(step4.output$precision.sim$DU_sim$ba_DU > action.level)
  }else{
    sum(step4.output$precision.sim$DU_sim$ba_DU < action.level)
  }
  
  sim_ct <- nrow(step4.output$accuracy.sim$DU_sim)
  
  outplot <- ggplot(step4results, aes(ba_DU)) +
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
               linewidth = 0.8, key_glyph = "path") +
    scale_linetype_manual(values = c(1, 2)) +
    scale_color_manual(values = c("red", "blue")) +
    labs(linetype = "", color = "") +
    xlab(paste0("Model-estimated measured EPC (mg kg-1)\nacross ",
               sim_ct, " model iterations")) +
    theme(axis.text = element_text( size = 14 ),
          axis.title = element_text( size = 16, face = "bold" ),
          strip.text = element_text(size = 20),
          legend.text=element_text(size = 14),
          legend.position = "top")
  
  accuracyText <- paste(
    "<b>Accuracy:</b> ", round(100*accuracy_ct/sim_ct, 1), 
    "% of the simulated measured bioavailability-adjusted EPCs (i.e., ",
    accuracy_ct, " out of ", sim_ct, " simulations) are ", 
    ifelse(error.type == "Type 1", "less than", "greater than"), 
    " the observed EPC assuming the DU's true EPC equals the AL.", sep = ""
  )
  
  precisionText <- paste(
    "<b>Precision:</b> ", round(100*precision_ct/sim_ct, 1),
    "% of the simulated measured bioavailability-adjusted EPCs (i.e., ",
    precision_ct, " out of ", sim_ct, " simulations) are ", 
    ifelse(error.type == "Type 1", "greater than", "less than"), 
    " the AL assuming the DU's true EPC equals the measured EPC.", sep = ""
  )
  
  return(list(outplot = outplot, accuracyText = accuracyText, precisionText = precisionText))
}


