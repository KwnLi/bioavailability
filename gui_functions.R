# GUI functions


step1_plot <- function(step1.output, error_threshold){
  frcAct <- step1.output$step1$frcAct[1]
  errortype <- ifelse(frcAct > 0, "False compliance", "False exceedance")
  
  step1results <- step1.output$step1 %>% 
    mutate(aggregation = ifelse(n_incr==1, "discrete", paste("Incr. = ", n_incr)))
  
  step1results$aggregation <- factor(step1results$aggregation,
                                     levels = unique(step1results$aggregation[order(step1results$n_incr)]))
  
  ggplot(step1results,
         aes(x = n_tot, y = err_pb*100, color = aggregation)) + 
    geom_line() + geom_point() + 
    xlab("Num. samples analyzed") + 
    ylab(paste("Probability of", errortype, "error", sep = " ")) +
    geom_hline(yintercept = error_threshold, color = "red") + 
    ggtitle(paste(errortype, " error with more sampling, when true \nbioavailable ", step1.output$AsPb, " is ",
                  round(abs(frcAct*100),1), "% ", 
                  ifelse(frcAct>0, "above", "below"), 
                  " action level",
                  sep = ""),) + 
    ylim(c(0, 100)) +
    theme_article() +
    theme(legend.position = c(0.95,0.95),
          legend.justification = c(1,1),
          axis.text = element_text( size = 14 ),
          axis.title = element_text( size = 16, face = "bold" ),
          legend.text=element_text(size = 14),
          plot.title = element_text(size = 20)
    )
}

step1a_plot <- function(step1a.output){
  measured.EPC <- step1a.output$err_pb$tru_ba
  action.level <- step1a.output$err_pb$actLvl.adj
  
  total.range <- range(step1a.output$DU_sim$ba_DU)
  
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
  
  hatchresults <- step1a.output$DU_sim %>%
    filter(if(measured.EPC < action.level){
      ba_DU <= action.level
    }else{
      ba_DU > action.level
    })
  
  correct_ct <- nrow(hatchresults)
  sim_ct <- nrow(step1a.output$DU_sim)
  
  outplot <- ggplot(step1a.output$DU_sim, aes(ba_DU)) +
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
    theme_article() +
    geom_vline(data = data.frame(threshold = c("Assumed EPC", "Action level"),
                                 value = c(measured.EPC, action.level)),
               mapping = aes(xintercept = value, linetype = threshold, color = threshold), 
               linewidth = 0.8, key_glyph = "path") +
    scale_linetype_manual(values = c(1, 2)) +
    scale_color_manual(values = c("red", "blue")) +
    labs(linetype = "", color = "") +
    xlab(paste0("Model-estimated assumed true EPC (mg kg-1)\nacross ",
                sim_ct, " model iterations")) +
    theme(axis.text = element_text( size = 14 ),
          axis.title = element_text( size = 16, face = "bold" ),
          strip.text = element_text(size = 20),
          legend.text=element_text(size = 14),
          legend.position = "top")
  
  outputText <- paste0(
    ifelse(measured.EPC < action.level,
           "<b>False compliance decision error probability = ", 
           "<b>False exceedance decision error probability = "
           ),
    round(100-(100*correct_ct/sim_ct), 1), "%:</b> ",
    "When the simulated mean EPC is ",
    abs(round(100*step1a.output$err_pb$frcAct, 1)),
    "% ", ifelse(measured.EPC < action.level, "less than", "greater than"),
    " the action level, ",
    round(100*correct_ct/sim_ct, 1), 
    "% of the simulated measured bioavailability-adjusted EPCs (i.e., ",
    correct_ct, " out of ", sim_ct, " simulations) are ", 
    ifelse(measured.EPC < action.level, "less than", "greater than"), 
    " the action level."
  )
  
  return(list(outplot, outputText))
  
}


step2_plot <- function(step2.output, error_threshold){
  errortype <- ifelse(median(step2.output$step2$frcAct) > 0, "False compliance", "False exceedance")
  
  ggplot(step2.output$step2, aes(x = 100*frcAct, y = err_pb*100)) + 
    geom_line() + geom_point() + 
    ylab(paste("Probability of", errortype, "error", sep = " ")) +
    xlab(paste0("% assumed true EPC ", ifelse(errortype == "False compliance", "above", "below"), " the action level")) +
    geom_hline(yintercept = error_threshold, color = "red") + 
    ggtitle(
      paste(errortype, " error with ", ifelse(errortype=="False compliance", "increasing", "decreasing"),
            "\nbioavailable ", step2.output$AsPb,
            ifelse(errortype=="False compliance", " above", " below"), " the action level",
                  sep = ""),) + 
    ylim(c(0,100)) +
    theme_article() +
    theme(axis.text = element_text( size = 14 ),
          axis.title = element_text( size = 16, face = "bold" ),
          legend.text=element_text(size = 14),
          plot.title = element_text(size = 20)
          )
}

# Step 2 text functions

# function to find interpolated value given two points (err_pb is the y)
interp <- function(x, y, middle.point.y){
  slope = (y[2]-y[1])/(x[2]-x[1])
  return(x[1] + (middle.point.y - y[1])/slope)
}

step2_text_t1 <- function(step2_t1, error_threshold){
  t1_thresh <- error_threshold/100
  
  type1_df <- step2_t1$step2 %>%
    mutate(from.t1 = err_pb - t1_thresh) %>% # find difference from t1 threshold
    mutate(dif.neg = from.t1 <= 0) %>% # sign of difference
    mutate(lag.neg = lag(dif.neg), lead.neg = lead(dif.neg))
  
  # sort by absolute difference from threshold and find 2 closest
  nearest.type1 <- type1_df %>% filter(from.t1 < 0) %>% filter(from.t1 == max(from.t1)) %>%
    bind_rows(type1_df %>% filter(from.t1 > 0) %>% filter(from.t1 == min(from.t1)))
  
  # find where error crosses threshold (false compliance/type1)
  if(nrow(nearest.type1)!=2){ # test if both same logical (==1)
    message.type1 <- " the sampling protocol evaluated does not achieve target false compliance (Type 1) decision error probabilities across the range in the DU's true EPC assessed."
  }else{
    frcAct.t1 <- interp(x=nearest.type1$frcAct, y=nearest.type1$err_pb, middle.point.y=t1_thresh)
    tru_ba.t1 <- interp(x=nearest.type1$tru_ba, y=nearest.type1$err_pb, middle.point.y=t1_thresh)
    
    message.type1 <- paste0(", so long as the DU's true bioavailability-adjusted EPC is greater than ",
                            round(tru_ba.t1, 0), " mg bioavailable ", step2_t1$AsPb, " (i.e., greater than ",
                            round(100*frcAct.t1, 1), "% above the ",
                            nearest.type1$actLvl.adj[1], " ppm AL), the sampling protocol evaluated will achieve satisfactory decision error probability goals for false compliance.")
  }
  
  return(paste0("<b>False compliance error:</b> the simulation estimates that", message.type1))
}

step2_text_t2 <- function(step2_t2, error_threshold){
  t2_thresh <- error_threshold/100
  type2_df <- step2_t2$step2 %>%
    mutate(from.t2 = err_pb - t2_thresh) %>% # find difference from t2 threshold
    mutate(dif.neg = from.t2 <= 0) %>% # sign of difference
    mutate(lag.neg = lag(dif.neg), lead.neg = lead(dif.neg))

  # sort by absolute difference from threshold and find 2 closest
  nearest.type2 <- type2_df %>% filter(from.t2 < 0) %>% filter(from.t2 == max(from.t2)) %>%
    bind_rows(type2_df %>% filter(from.t2 > 0) %>% filter(from.t2 == min(from.t2)))
  
  # find where error crosses threshold (false exceedance/type 2)
  if(nrow(nearest.type2)!=2){ # test if both same logical (==1)
    message.type2 <- " the sampling protocol evaluated does not achieve target false exceedance (Type 2) decision error probabilities across the range in the DU's true EPC assessed."
  }else{
    frcAct.t2 <- interp(x=nearest.type2$frcAct, y=nearest.type2$err_pb, middle.point.y=t2_thresh)
    tru_ba.t2 <- interp(x=nearest.type2$tru_ba, y=nearest.type2$err_pb, middle.point.y=t2_thresh)
    
    message.type2 <- paste0(", so long as the DU's true bioavailability-adjusted EPC is less than ",
                           round(tru_ba.t2, 0), " mg bioavailable ", step2_t2$AsPb, " (i.e., less than ",
                           round(100*abs(frcAct.t2), 1), "% below the ",
                           nearest.type2$actLvl.adj[1], " ppm AL), the sampling protocol evaluated will achieve satisfactory decision error probability goals for false exceedance")
  }

  return(paste0("<b>False exceedance error:</b> the simulation estimates that", message.type2))
}

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
  ) %>% bind_rows(.id = "Simulation")
  
  hatchresults <- step4results %>%
    filter(if(error.type == "False compliance"){
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
    ifelse(error.type == "False compliance", "less than", "greater than"), 
    " the observed EPC assuming the DU's true EPC equals the AL.", sep = ""
  )
  
  precisionText <- paste(
    "<b>Precision:</b> ", round(100*precision_ct/sim_ct, 1),
    "% of the simulated measured bioavailability-adjusted EPCs (i.e., ",
    precision_ct, " out of ", sim_ct, " simulations) are ", 
    ifelse(error.type == "False compliance", "greater than", "less than"), 
    " the AL assuming the DU's true EPC equals the measured EPC.", sep = ""
  )
  
  return(list(outplot = outplot, accuracyText = accuracyText, precisionText = precisionText))
}


