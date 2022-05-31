library(ggplot2)
library(truncnorm) 
library(dplyr)

#####Simulate error function#####
simError_site <- function(
  # Are you modeling Pb or As?
  AsPb = "Pb",        # "As" or "Pb"
  
  # What is the site-specific soil contaminant action level?
  actLvl = 400,       # mg/kg  (action level: limit of biovalability above which site has to be remedied)
  
  # Use mean of upper 95% interval in estimation of total contaminant?
  useMeanTot = T,       # T = use the mean; F = use 95% interval
  
  # Use mean or upper 95% interval in estimation of IVBA?
  useMeanIVBA = T,      # T = use the mean; F = use 95% interval
  
  # Were the samples composited?
  compositeTF = F,
  
  # If yes, how many increments were combined per composite?
  Xaggr = 1,         # composite cells to aggregate
  
  # What heterogeneity should be used in the simulation?
  heterogeneity = "user",   # "user" = user-supplied heterogeneity
                            # "default" = use default heterogeneity values
                            # "sample" = use the sample hetergeneity
  
  # Should heterogeneity simulation be based on mean or 95% upper level?
  useHetMnTF = TRUE,        # TRUE = use mean heterogeneity; FALSE = use upper 95%
  
  ##### INPUTS FOR DU SIMULATION
  # fraction above/below
  tot_n = NULL,  # how many total metal concentration samples?
  IVBA_n = NULL, # how many samples analyzed for IVBA?
  
  frcAct = NULL,    # fraction of the action level threshold
  
  RBAmean = 60,     # assume RBA value
  
  # coefficient of variation for DUs. Keep same as the site
  # CoeV_tot = NULL,     # assume coefficient of variation for total metal conc.
  # CoeV_RBA = NULL,    # assume coefficient of variation for RBA
  
  ##### SITE-WIDE INPUTS
  siteDU_n = NULL,       # number of DU to simulate in each site
  
  # Site-level coefficient of variation
  site_CoeV_tot = NULL, # coefficient of variation of total concentration among site DUs
  site_CoeV_RBA = NULL, # coefficient of variation of RBA among site DUs
  
  ###### SIMULATION PARAMETERS
  error_tot = FALSE,    # include total conc. measurement error?
  ivba_model = FALSE,   # include IVBA model error?
  dist_tot = "lognorm", # distribution of total concentration
  dist_RBA = "normal",  # distribution of RBA
  iter = 1000,       # nr. SITE simulations
  DU_iter = 1000     # nr. DU simulations
){
  
  # Warnings about parameter combinations
  simWarnings <- c()
  # A warning message if >1 aggregated samples is selected but compositing is not selected
  if(Xaggr>1 & !compositeTF){
    simWarnings <- append(simWarnings, "Warning: Ignoring aggregated cell number since you indicated no compositing")
  }
  if(Xaggr==1 & compositeTF){
    compositeTF <- F
    simWarnings <- append(simWarnings, "Warning: Composites of 1 sample are treated as discrete")
  }
  
  # Define "true" total and RBA for site
  tru_mu_rba <- RBAmean
  tru_mu_tot <- ((frcAct*actLvl) + actLvl)/(tru_mu_rba/100)
  
  # relative sdv of RBA and total metal (based on what measurements?)
  if(heterogeneity == "user"){
    site_CoeV_RBA <- site_CoeV_RBA
    site_CoeV_tot <- site_CoeV_tot
  }else if(heterogeneity == "default"){  # if using default mean heterogeneity
    if(AsPb == "Pb"){
      site_CoeV_RBA <- ifelse(useHetMnTF, 0.55, 0.95) # use mean value, else use 95% level
      site_CoeV_tot <- ifelse(useHetMnTF, 1.12, 1.52)
    } else if(AsPb == "As"){
      site_CoeV_RBA <- ifelse(useHetMnTF, 1.118799762, 0.452062439)
      site_CoeV_tot <- ifelse(useHetMnTF, 0.452062439, 1.118799762)
    }
  }
  
  # Functions to generate "true" total conc. and rba data in DU
  if(dist_tot == "normal"){
    tru_tot <- function(n.totmeas, mn.tot){
      rnorm(n=n.totmeas, mean=mn.tot, sd=mn.tot*site_CoeV_tot)
      }
  } else if(dist_tot == "lognorm"){
    tru_tot <- function(n.totmeas, mn.tot){
      custom_rlnorm(n=n.totmeas, m=mn.tot, s=mn.tot*site_CoeV_tot)
      }
  } else{
    stop(paste("Unrecognized distribution for total concentration: ",
               dist_tot, sep = ""))
  }
  
  if(dist_RBA == "normal"){
    tru_rba <- function(n.rbameas, mn.rba){
      rtruncnorm(n=n.rbameas, a = 0, b = 100,
                 mean=mn.rba, sd=mn.rba*site_CoeV_RBA)
      } 
  } else if(dist_RBA == "lognorm"){
    tru_rba <- function(n.rbameas, mn.rba){
      custom_rlnorm(n=n.rbameas, m=mn.rba, s=mn.rba*site_CoeV_RBA, upper = 100)
      } 
  } else if(dist_RBA == "uniform"){
    tru_rba <- function(n.rbameas){
      runif(n=n.rbameas, min = 0, max = 1)
      }
  } else{
    stop(paste("Unrecognized distribution for RBA: ",
               dist_RBA, sep = ""))
  }
  
  # initialize output list
  decision_error <- list()
  
  # prd_ba <- mx_errYN # predicted bioavailable metal (mg/kg) for the decision unit
  # 
  # empty dataframes for data checking
  samp.DUsample = NULL
  samp.meas_tot = NULL
  samp.meas_ivb = NULL
  samp.prd_ba = NULL
  
  # I) Create realistic true total metal and RBA 
  #    [i.e. populate synthetic decision unit]
  
  for (k in 1:iter){
    
    # generate plausible DU means
    sim_DU_means_k <- cbind(
      DU_tot = tru_tot(n.totmeas = siteDU_n, mn.tot = tru_mu_tot),
      DU_rba = tru_rba(n.rbameas = siteDU_n, mn.rba = tru_mu_rba)
      )
    
    # if number of aggregated samples > 1, then mix samples of cells for measurement input
    # else, treat each sample as a separate measurement input
    
    raw_tot <- list()
    raw_rba <- list()
    
    for(i in 1:siteDU_n){
      raw_tot[[i]] <- expand.grid(
        DU_id = i,
        DU_sample = 1:tot_n,
        comp = 1:Xaggr
      ) %>%
        bind_cols(
          matrix(
            tru_tot(DU_iter*tot_n*Xaggr, mn.tot = sim_DU_means_k[i,"DU_tot"]),
            nrow = tot_n*Xaggr,
            dimnames = list(c(), paste("iter", 1:DU_iter, sep = "_"))
          )
        )
      
      raw_rba[[i]] <- expand.grid(
        DU_id = i,
        DU_sample = 1:IVBA_n,
        comp = 1:Xaggr
      ) %>%
        bind_cols(
          matrix(
            tru_rba(DU_iter*IVBA_n*Xaggr, mn.rba = sim_DU_means_k[i,"DU_rba"]),
            nrow = IVBA_n*Xaggr,
            dimnames = list(c(), paste("iter", 1:DU_iter, sep = "_"))
          )
        )
    }
    
    raw_tot <- bind_rows(raw_tot)
    raw_rba <- bind_rows(raw_rba)
    
    if(compositeTF){ # IF COMPOSITING
      measurement_input_tot <- raw_tot %>% group_by(DU_id, DU_sample) %>% select(-comp) %>%
        dplyr::summarize(across(.fns = mean), .groups = "drop")
      
      measurement_input_rba <- raw_rba %>% group_by(DU_id, DU_sample) %>% select(-comp) %>%
        dplyr::summarize(across(.fns = mean), .groups = "drop")
    }else{ # IF DISCRETE
      measurement_input_tot <- raw_tot %>% select(-comp)
      measurement_input_rba <- raw_rba %>% select(-comp)
    }
    
    # III) (lab) measure total metal for each input, 
    #      1 time per cell
    if(error_tot){
      meas_tot <- mutate(measurement_input_tot, 
                         across(starts_with("iter"), 
                                ~ rtruncnorm(n=1, a=0, b=Inf, mean=.x, sd=.x*5/100)))
    }else{
      meas_tot <- measurement_input_tot
    }

    
    # measure IVBA for [IVBAsamp] measurement_input (i.e. run inverse model), 
    # 1 time per cell
    meas_ivb <- mutate(measurement_input_rba, 
                       across(starts_with("iter"), 
                              ~ fxy(input = .x, input.type = "RBA", metal = AsPb, model.error = ivba_model)))
    
    # NEXT STEP: CALCULATE DU MEANS/95CI OVER ITERATIONS - use grouping and across()
    
    # IV, V) avg IVBA measurements, calculate DU RBA
    if(useMeanIVBA){   # if user wants to use the mean value
      est_rba_DU <- meas_ivb %>% group_by(DU_id) %>% 
        summarize(across(starts_with("iter"), .fns = mean), .groups = "drop") %>%
        mutate(across(starts_with("iter"), ~ fxy(input = .x, input.type = "IVBA", metal = AsPb)))
    }else{         # if user wants to use the 95% inverval value of IVBA measurements
      est_rba_DU <- meas_ivb %>% group_by(DU_id) %>% 
        summarize(across(starts_with("iter"), ~ upper95(.x, lvl = 0.975)), .groups = "drop") %>%
        mutate(across(starts_with("iter"), ~ fxy(input = .x, input.type = "IVBA", metal = AsPb)))
    }
    
    # VI) calc bioaval total contaminant mass fraction (mg/kg) for DU 
    if(useMeanTot){   # if user wants to use the mean value
      est_tot_DU <- meas_tot %>% group_by(DU_id) %>% 
        summarize(across(starts_with("iter"), .fns = mean), .groups = "drop")
    }else{         # if user wants to use the 95% inverval value of total
      est_tot_DU <- meas_tot %>% group_by(DU_id) %>% 
        summarize(across(starts_with("iter"), ~ upper95(.x, lvl = 0.975)), .groups = "drop")
    }
    
    ba_DU <- est_tot_DU %>% select(starts_with("iter")) * est_rba_DU %>% select(starts_with("iter"))/100
    # exceeding threshold in this simulation, Y/N?
    if (frcAct>0){
      decision_error[[k]] <- rowSums(ba_DU<actLvl)/DU_iter # for t1 error
    }else{
      decision_error[[k]] <- rowSums(ba_DU>actLvl)/DU_iter # for t2 error
    }
    
    # store bioavailable metal for DU for this simulation (not currently implemented)
    # samp.prd_ba[[k]] <- ba_DU
    
    # output for error checking:
    if(k==1){
      samp.DUsample = 
        bind_rows(samp.DUsample, 
                  data.frame(iteration = k, value = "total", measurement_input_tot),
                  data.frame(iteration = k, value = "rba", measurement_input_rba)
        )
      samp.meas_tot = bind_rows(samp.meas_tot, data.frame(iteration = k, meas_tot))
      samp.meas_ivb = bind_rows(samp.meas_ivb, data.frame(iteration = k, meas_ivb))
      samp.prd_ba = bind_rows(samp.prd_ba, data.frame(iteration = k, est_tot_DU %>% select(!starts_with("iter")), ba_DU))
    }
    
  } # loop over sim
  
  err_pb <- data.frame(
    iteration = 1:iter,
    matrix(unlist(decision_error), ncol = siteDU_n, byrow = T,
                          dimnames = list(c(), paste("DU", 1:siteDU_n, sep = "_")))
  )
  
  return(list(frcAct = frcAct, 
              tru_mu_tot = tru_mu_tot, 
              tru_mu_rba = tru_mu_rba, 
              errortype = ifelse(frcAct>0, "type 1", "type 2"),
              err_pb = err_pb,
              tot_n = tot_n,
              IVBA_n = IVBA_n,
              sim_attributes = list(AsPb = AsPb,
                                    actLvl = actLvl,
                                    compositeTF = compositeTF,
                                    Xaggr = Xaggr, 
                                    useMeanTot = useMeanTot, 
                                    useMeanIVBA = useMeanIVBA,
                                    heterogeneity = heterogeneity,
                                    Hetvals = c(site_CoeV_tot = site_CoeV_tot, site_CoeV_RBA = site_CoeV_RBA),
                                    iter = iter,
                                    simWarnings = simWarnings,
                                    samp.DUsample = samp.DUsample,
                                    samp.meas_tot = samp.meas_tot, 
                                    samp.meas_ivb = samp.meas_ivb,
                                    samp.prd_ba = samp.prd_ba)))
}


#####Custom lognormal#####
# just a wrapper form the rlnorm that converts the normal mean and sd
# inputs so that the output lognormal samples have the specified mean and sd
# at the lognormal scale

custom_rlnorm <- function(n, m, s, upper = Inf){
  location <- log(m^2 / sqrt(s^2 + m^2))
  shape <- sqrt(log(1 + (s^2 / m^2)))
  
  output <- rlnorm(n=n, location, shape)
  
  if(!is.infinite(upper)){
    output[output > upper] <- upper
  }
  return(return(output))
}

#####Output text#####
simText <- function(simResult){
  
  # Extract simulation variables
  AsPb = simResult$sim_attributes$AsPb
  frcAct = simResult$frcAct
  tru_mu_rba = simResult$tru_mu_rba
  tru_mu_tot = simResult$tru_mu_tot
  useMeanTot = simResult$sim_attributes$useMeanTot
  actLvl = simResult$sim_attributes$actLvl
  err_pb = simResult$err_pb
  errortype = simResult$errortype
  
  # Output text
  return(
    paste("Based on the sampling protocol and selected decision unit assumptions, with an actual bioavailable contaminant level ",
          round(abs(frcAct[1]*100), 1), "% ", ifelse(frcAct[1]>0, "above", "below"), 
          " the action level, the probability of making a ",
          errortype, " error is <b>", round(err_pb[1,"probability"], 1), "%</b>.",
          sep = "")
  )
}

#####Conversion function######
fxy <- function(input = NULL,
                input.type = NULL,  # IVBA or RBA
                metal = NULL,
                model.error = T
){
  
  if(is.null(input)){stop("Missing input values")}
  if(is.null(input.type)){stop("Missing input type")}
  
  # m and b depend on the contaminant:
  if(metal == "As"){
    sepred = 19/1.96 # from 95% prediction limit for a single As RBA measurement 
    m = 0.79         # from https://doi.org/10.1080/15287394.2015.1134038
    b = 3
  }else if(metal == "Pb"){
    sepred = 32/1.96 # from 95% prediction limit for a single Pb RBA measurement 
    m = 0.878       # from OSWER 9285.7-77 (May 2007)
    b = -2.81
  }else{
    stop("no valid metal provided")
  }
  
  if(input.type == "RBA"){  # formerly "y"
    # inverse model to calculate observed IVBA from true RBA 
    if(model.error){
      out = (input - rnorm(n = length(input), mean = 0, sd = sepred) - b)/m
    }else{
      out = (input - b)/m
    }
    
    out[out<0] <- 0  # make negative values 0
    out[out>100] <- 100  # make values over 100 into 100
    return(out)
  }else if (input.type == "IVBA"){  # formerly "x"
    # direct model, from mean measured IVBA to estimated RBA
    out = m*input+b
    
    out[out<0] <- 0  # make negative values 0
    out[out>100] <- 100  # make values over 100 into 100
    return(out)
  }
  
}
# define inverse model to calculate observed IVBA from true RBA 
# fy <- function(y, m, b) {
#   x = (y - rnorm(1,0,sepred) - b)/m
#   return(max(0,x))}

# define direct model, from mean measured IVBA to estimated RBA
# fx <- function(x, m, b) { 
#   y = m*x+b
#   return(y)}


#####Find upper 95% CI######
# upper 95% conf. int. of the mean function:
upper95 <- function(x,lvl){
  xbar = mean(x)
  up95 = xbar + qnorm(lvl)*(sd(x)/sqrt(length(x)))
  return(up95)
}


#####Output plot#####
# Plot the error curve results by number of samples with red line for error threshold

simPlot <- function(simResult){
  
  # Extract simulation variables
  frcAct = simResult$frcAct
  tot_n = simResult$tot_n
  IVBA_n = simResult$IVBA_n
  err_pb = simResult$err_pb
  AsPb = simResult$sim_attributes$AsPb
  errortype = simResult$errortype
  
  xoffset <- ifelse(tot_n > IVBA_n, tot_n, IVBA_n)
  
  ggplot(err_pb, aes(x = simparam - xoffset, y = probability)) + geom_line() + geom_point() + 
    xlab("Number of additional samples analyzed for both total concentraiton and IVBA") + 
    ylab(paste("Probability of", errortype, "error", sep = " ")) +
    geom_hline(yintercept = ifelse(errortype == "type 1", 5, 20), color = "red") + 
    ggtitle(paste("Change in probability of ",errortype, " error when true bioavailable ", AsPb, " is ",
                  round(abs(frcAct*100),1), "% ", 
                  ifelse(frcAct>0, "above", "below"), " the action level, \nper increase in samples analyzed for total metal and IVBA values",
                  sep = ""),) + 
    theme_bw()
}

simPlot2 <- function(simResult){
  
  # Extract simulation variables
  frcAct = simResult$frcAct[-1]
  err_pb = simResult$err_pb[-1,]
  AsPb = simResult$sim_attributes$AsPb
  errortype = simResult$errortype
  
  ggplot(err_pb, aes(x = simparam*100, y = probability)) + geom_line() + geom_point() +
    xlab(paste("%", ifelse(frcAct>0, "above", "below"), "the action level")) + 
    ylab(paste("Probability of", errortype, "error", sep = " ")) +
    geom_hline(yintercept = ifelse(errortype == "type 1", 5, 20), color = "red") + 
    ggtitle(paste("Change in probability of ",errortype, " error \nwhen true bioavailable ", AsPb, " ranges from ",
                  round(min(abs(frcAct*100)),1), " to ", round(max(abs(frcAct*100)),1), "% ", 
                  ifelse(frcAct>0, "above", "below"), " the action level",
                  sep = ""),) +
    theme_bw()
}

#####Plot title#####
simTabTitle <- function(simResult){
  
  # Extract error type
  errortype = simResult$errortype
  
  # Output text
  return(
    paste(ifelse(errortype=="type 1", "Type 1 ", "Type 2 "), "error simulation results",
          sep = "")
  )
}

#####Precision plot######
# Plot distribution of bioavailability predictions and 95% interval

precPlot <- function(simResult, plot.median = FALSE){
  
  # Extract simulation variables
  frcAct = simResult$frcAct
  iter = simResult$sim_attributes$iter
  ba = simResult$sim_attributes$samp.prd_ba[1:iter,]
  actLvl = simResult$sim_attributes$actLvl
  
  # derived values
  ba_95 = quantile(ba$ba_DU, probs=c(.025,.975))
  ba_mn = mean(ba$ba_DU)
  ba_md = median(ba$ba_DU)
  
  outplot <- ggplot(ba, aes(ba_DU)) + geom_histogram(fill = "cornsilk3") + 
    theme_bw() +
    xlab("Predicted bioavailability") + ylab("Result frequency") +
    geom_vline(xintercept = ba_95["2.5%"], color = "red", lty = 2) +
    geom_vline(xintercept = ba_95["97.5%"], color = "red", lty = 2) +
    geom_vline(xintercept = ba_mn, color = "red") +
    geom_vline(xintercept = actLvl, color = "blue") +
    geom_hline(yintercept = 0, color = "black")
  
  # for plotting
  plot.ctmax <- max(ggplot_build(outplot)$data[[1]]$count)
  plot.xmax <- max(ggplot_build(outplot)$data[[1]]$x)
  
  # add the line labels
  outplot <- outplot + 
    annotate(geom = "text", 
             x = ba_95["2.5%"] - (plot.xmax/50), 
             y = plot.ctmax/2, 
             label = paste("2.5% (", round(ba_95["2.5%"], 1), ")", sep = ""), color = "red",
             angle = 90) + 
    annotate(geom = "text", 
             x = ba_95["97.5%"] - (plot.xmax/50), 
             y = plot.ctmax/2, 
             label = paste("97.5% (", round(ba_95["97.5%"], 1), ")", sep = ""), color = "red",
             angle = 90) + 
    annotate(geom = "text", 
             x = ba_mn - (plot.xmax/50), 
             y = plot.ctmax/2, 
             label = paste("Mean =", round(ba_mn, 1), sep = " "), color = "red",
             angle = 90) + 
    annotate(geom = "text", 
             x = actLvl - (plot.xmax/50), 
             y = plot.ctmax/2, 
             label = paste("Action level =", actLvl, sep = " "), color = "blue",
             angle = 90)
  
  if(plot.median){
    outplot <- outplot +
      geom_vline(xintercept = ba_md, color = "blue", lty = 2)
  }
  return(outplot)
  
}

#####Precision title#####
precisionTabTitle <- function(simResult){
  
  # Extract error type
  errortype = simResult$errortype
  
  # Output text
  return(
    paste(ifelse(errortype=="type 1", "Type 1 ", "Type 2 "), "bioavailability estimate precision",
          sep = "")
  )
}

#####Precision title#####
resultTabTitle <- function(simResult){
  
  # Extract error type
  errortype = simResult$errortype
  
  # Output text
  return(
    paste("Possibility of ", ifelse(errortype=="type 1", "Type 1 ", "Type 2 "), "error",
          sep = "")
  )
}

#####Custom Numeric Input#####
numericInputRow <- function(inputId, label, value = NULL, step = NULL, max = NULL, min = NULL) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "numeric", value = value, 
                 step = step, max = max, min = min,
                 class="input-small"))
}

test <- simError_site(siteDU_n = 5, tot_n = 5, IVBA_n = 3, compositeTF = T, Xaggr = 3, 
                      site_CoeV_tot = 0.5, frcAct = 0.25, site_CoeV_RBA = 0.05,
                      useMeanTot = T, error_tot = T, ivba_model = T,
                      iter = 10)
