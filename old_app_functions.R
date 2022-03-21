library(ggplot2)
library(truncnorm) 
library(dplyr)

#####Simulate error function#####
simError <- function(
  # Type of simulation: vary number of samples or contaminant level?
  simChoice = "sample", # "sample" or "contaminant"
  errorType = "I",      # "I" or "II"
  
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
  
  ##### INPUTS FOR ORIGINAL SAMPLING PROTOCOL
  # fraction above/below
  tot_n = NULL,  # how many total metal concentration samples?
  IVBA_n = NULL, # how many samples analyzed for IVBA?
  
  frcAct = NULL,    # fraction of the action level threshold
  minFrcAct = NULL, # minimum fraction of action level to simulate (simChoice = "contaminant")
  maxFrcAct = NULL, # maximum fraction of action level to simulate (simChoice = "contaminant")
  
  CoeV_tot = NULL,     # assume coefficient of variation for total metal conc.
  
  RBAmean = 60,     # assume RBA value
  CoeV_RBA = NULL,    # assume coefficient of variation for RBA
  
  ###### SIMULATION PARAMETERS
  error_tot = FALSE,    # include total conc. measurement error?
  ivba_model = FALSE,   # include IVBA model error?
  dist_tot = "lognorm", # distribution of total concentration
  dist_RBA = "normal",  # distribution of RBA
  iter = 1000,          # nr. simulations
  sampmax = NULL,       # maximum number of samples to simulate (simChoice = "sample")
  numbins = NULL        # nr. divisions over range of contaminant levels (simChoice = "contaminant")
  
){
  
  # define inverse model to calculate observed IVBA from true RBA 
  if(ivba_model){
    fy <- function(y, m, b) {
      x = (y - rnorm(1,0,sepred) - b)/m
      return(max(0,x))}
  }else{
    fy <- function(y, m, b) {
      x = (y - b)/m
      return(max(0,x))}
  }
  
  # define direct model, from mean measured IVBA to estimated RBA
  fx <- function(x, m, b) { 
    y = m*x+b
    return(y)}
  
  # m and b depend on the contaminant:
  if(AsPb == "As"){
    sepred = 19/1.96 # from 95% prediction limit for a single As RBA measurement 
    m = 0.79         # from https://doi.org/10.1080/15287394.2015.1134038
    b = 3
  }else if(AsPb == "Pb"){
    sepred = 32/1.96 # from 95% prediction limit for a single Pb RBA measurement 
    m = 0.878       # from OSWER 9285.7-77 (May 2007)
    b = -2.81
  }
  
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
  
  # function to calculate coefficient of variance
  # cv <- function(x){sd(x)/mean(x)} 
  
  # relative stdv of RBA and total metal (based on what measurements?)
  if(heterogeneity == "user"){
    CoeV_RBA <- CoeV_RBA
    CoeV_tot <- CoeV_tot
  }else if(heterogeneity == "default"){  # if using default mean heterogeneity
    if(AsPb == "Pb"){
      CoeV_RBA <- ifelse(useHetMnTF, 0.55, 0.95) # use mean value, else use 95% level
      CoeV_tot <- ifelse(useHetMnTF, 1.12, 1.52)
    } else if(AsPb == "As"){
      CoeV_RBA <- ifelse(useHetMnTF, 1.118799762, 0.452062439)
      CoeV_tot <- ifelse(useHetMnTF, 0.452062439, 1.118799762)
    }
  }
  # else if(heterogeneity == "sample"){  # if calculating from input data
  #   CoeV_RBA <- IVBA_meas %>% fx(m=m, b=b) %>% cv()
  #   CoeV_tot <- tot %>% cv()
  # }
  
  # Differing settings depending on whether varying sample nr. or contaminant level
  sampstart <- ifelse(tot_n > IVBA_n, tot_n, IVBA_n)
  if(simChoice == "sample"){
    Ysamp <- c(sampstart:(sampstart+sampmax)) # vector of possible sample numbers, starting from user-supplied value
    sim.num <- length(Ysamp)        # parameter for creating output matrix
    dim.x <- Ysamp                  # parameter for creating output matrix
  }else if (simChoice == "contaminant"){
    Ysamp <- sampstart              # only simulate user-supplied sampling protocol for "contaminant"
    frcAct <- c(frcAct, round(seq(from = ifelse(minFrcAct>-1,minFrcAct,-0.99), 
                                  to = ifelse(maxFrcAct>-1,maxFrcAct,-0.01), 
                                  length.out = numbins), 2))
    sim.num <- length(frcAct)       # parameter for creating output matrix
    dim.x <- frcAct                 # parameter for creating output matrix
  }
  
  # initialize output matrices 
  mx_errYN <- array(rep(NA, sim.num*iter),
                    dim=c(sim.num, iter),
                    dimnames = list(dim.x, paste("i=",1:iter,sep=""))) # matrix of hit(Yes error)/misses(No error)
  
  prd_ba <- mx_errYN # predicted bioavailable metal (mg/kg) for the decision unit
  
  # empty dataframe lists for data checking
  samp.DUsample = NULL
  # samp.meas_tot = NULL
  # samp.meas_ivb = NULL
  samp.prd_ba = NULL
  samp.mn = NULL
  cells <- list()
  
  
  ### SIMULATIONS ###
  
  # Define "true" DU values
  tru_mu_rba <- RBAmean
  tru_mu_tot <- NULL     # defined later
  
  # I) Create realistic true total metal and RBA based on fraction above action level
  #    [i.e. populate synthetic decision unit]
  for (i in 1:length(frcAct)){
    print(paste("Simulating fraction from action level =",frcAct[i]))
    
    tru_mu_tot[i] <- ((frcAct[i]*actLvl) + actLvl)/(tru_mu_rba/100)
    
    # generate "true" total conc. and rba data in DU
    if(dist_tot == "normal"){
      tru_tot <- function(n.totmeas){rnorm(n=n.totmeas, mean=tru_mu_tot[i], sd=tru_mu_tot[i]*CoeV_tot)}
    } else if(dist_tot == "lognorm"){
      tru_tot <- function(n.totmeas){custom_rlnorm(n=n.totmeas, m=tru_mu_tot[i], s=tru_mu_tot[i]*CoeV_tot)}
    } else{
      stop(paste("Unrecognized distribution for total concentration: ",
                 dist_tot, sep = ""))
    }
    
    if(dist_RBA == "normal"){
      tru_rba <- function(n.rbameas){rnorm(n=n.rbameas, mean=tru_mu_rba, sd=tru_mu_rba*CoeV_RBA)} 
    } else if(dist_RBA == "lognorm"){
      tru_rba <- function(n.rbameas){custom_rlnorm(n=n.rbameas, m=tru_mu_rba, s=tru_mu_rba*CoeV_RBA)} 
    } else if(dist_RBA == "uniform"){
      tru_rba <- function(n.rbameas){runif(n=n.rbameas, min = 0, max = 1)}
    } else{
      stop(paste("Unrecognized distribution for RBA: ",
                 dist_RBA, sep = ""))
    }
    
    # estimate bioavailable metal and remediation error
    for (j in 1:length(Ysamp)){ # varying amount of samples 
      print(paste("Simulating samples =",Ysamp[j]))
      
      # II) create Ysamp[j] aggregate(i.e. composite) or discrete measurement inputs 
      #     For aggregated cells (Xaggr>1), mix the content of Xaggr cells for each sample
      for (k in 1:iter){
        
        measurement_input <- cbind(tru_tot = rep(NA, Ysamp[j]), tru_rba = rep(NA, Ysamp[j]))
        # if number of aggregated samples > 1, then mix samples of cells for measurement input
        # else, treat each sample as a separate measurement input
        if(compositeTF){ # IF COMPOSITING
          for (z in 1:Ysamp[j]){
            measurement_input[z,"tru_tot"] <- mean(tru_tot(Xaggr))
            measurement_input[z,"tru_rba"] <- mean(tru_rba(Xaggr))
            # mix their properties to create Ysamp[j] new cells
          } # these are now our Ysamp[j] random cells
        }else{ # IF DISCRETE
          measurement_input[,"tru_tot"] = tru_tot(Ysamp[j])
          measurement_input[,"tru_rba"] = tru_rba(Ysamp[j])
        }
        
        # III) (lab) measure total metal for each input, 
        #      1 time per cell
        totsamp <- tot_n + j - 1 # increase tot_n by 1 each sample increase
        if(error_tot){
          meas_tot <- rtruncnorm(n=tot_n, a=0, b=Inf,
                                 mean = measurement_input[,"tru_tot"],
                                 sd = mean(measurement_input[,"tru_tot"])*.05)
        }else{
          meas_tot <- measurement_input[, "tru_tot"]  # directly use total concentration
        }
        
        # measure IVBA for [IVBAsamp] measurement_input (i.e. run inverse model), 
        # 1 time per cell
        IVBAsamp <- IVBA_n + j - 1  # increase IVBA by 1 each sample increase
        meas_ivb <- sapply(measurement_input[1:IVBAsamp,"tru_rba"], fy, m=m, b=b)  # convert RBA to IVBA
        
        # IV, V) avg IVBA measurements, calculate DU RBA
        if(useMeanIVBA){   # if user wants to use the mean value
          est_rba_DU <- meas_ivb %>% mean() %>% fx(m=m, b=b)
        }else{         # if user wants to use the 95% interval value of IVBA measurements
          est_rba_DU <- meas_ivb %>% upper95(lvl=.975) %>% fx(m=m, b=b)
        }
        
        # VI) calc bioaval total contaminant mass fraction (mg/kg) for DU 
        if(useMeanTot){   # if user wants to use the mean value
          est_tot_DU <- mean(meas_tot)
        }else{         # if user wants to use the 95% inverval value of total
          est_tot_DU <- upper95(meas_tot, lvl=.975)
        }
        ba_DU <- est_tot_DU*est_rba_DU/100
        
        # VII) Outputs
        out.index <- ifelse(simChoice=="sample",j,i)
        
        # exceeding threshold in this simulation, Y/N?
        if (frcAct[1]>0){
          mx_errYN[out.index,k] <- as.numeric(ba_DU<actLvl) # for t1 error
        }else{
          mx_errYN[out.index,k] <- as.numeric(ba_DU>actLvl) # for t2 error
        }
        
        # store bioavailable metal for DU for this simulation
        prd_ba[out.index,k] <- ba_DU
        
        # output for error checking:
        if(out.index==1 | out.index == sim.num){
          samp.DUsample = 
            bind_rows(samp.DUsample, 
                      data.frame(iteration = k, n_tot = totsamp, n_rba = IVBAsamp, frcAct = frcAct[i], 
                                 measurement_input, 
                                 meas_tot = meas_tot[1:max(totsamp, IVBAsamp)],
                                 meas_ivb = meas_ivb[1:max(totsamp, IVBAsamp)]) %>% 
                        mutate(tru_rba = replace(tru_rba, -(1:IVBAsamp), NA)) %>% # replace all not sampled with NA
                        mutate(tru_tot = replace(tru_tot, -(1:totsamp), NA))       # replace all not sampled with NA
            )
          # samp.meas_tot = bind_rows(samp.meas_tot, data.frame(iteration = k, n_tot = totsamp, n_rba = IVBAsamp, frcAct = frcAct[i], meas_tot))
          # samp.meas_ivb = bind_rows(samp.meas_ivb, data.frame(iteration = k, n_tot = totsamp, n_rba = IVBAsamp, frcAct = frcAct[i], meas_ivb))
          samp.prd_ba = bind_rows(samp.prd_ba, data.frame(iteration = k, 
                                                          n_tot = totsamp, n_rba = IVBAsamp, 
                                                          est_rba_DU, est_tot_DU,
                                                          frcAct = frcAct[i], ba_DU))
        }
        if(out.index==1){
          samp.mn = bind_rows(samp.mn,
                              data.frame(iteration = k, 
                                         mn_tot = mean(measurement_input[,"tru_tot"]),
                                         mn_rba = mean(measurement_input[,"tru_rba"])
                              )
          )
        }
        
      } # loop over sim
    } # ... over nr samples
  } # ... over contaminant levels
  
  err_pb <- data.frame(simparam = as.numeric(dimnames(mx_errYN)[[1]]),
                       probability =apply(mx_errYN, 1, mean)*100 # transform Y/N into prob of type I or II errors
  )
  
  return(list(simChoice = simChoice,
              frcAct = frcAct, 
              tru_mu_tot = tru_mu_tot, 
              tru_mu_rba = tru_mu_rba, 
              errortype = ifelse(frcAct[1]>0, "type 1", "type 2"),
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
                                    dist_tot = dist_tot,
                                    dist_RBA = dist_RBA,
                                    Hetvals = c(CoeV_tot = CoeV_tot, CoeV_RBA = CoeV_RBA),
                                    iter = iter, 
                                    sampmax = sampmax,
                                    simWarnings = simWarnings,
                                    samp.mn = samp.mn,
                                    samp.DUsample = samp.DUsample,
                                    # samp.meas_tot = samp.meas_tot, 
                                    # samp.meas_ivb = samp.meas_ivb,
                                    samp.prd_ba = samp.prd_ba)))
}

# upper 95% conf. int. of the mean function:
upper95 <- function(x,lvl){
  xbar = mean(x)
  up95 = xbar + qnorm(lvl)*(sd(x)/sqrt(length(x)))
  return(up95)
}

#####Custom lognormal#####
# just a wrapper form the rlnorm that converts the normal mean and sd
# inputs so that the output lognormal samples have the specified mean and sd
# at the lognormal scale

custom_rlnorm <- function(n, m, s){
  location <- log(m^2 / sqrt(s^2 + m^2))
  shape <- sqrt(log(1 + (s^2 / m^2)))
  return(rlnorm(n=n, location, shape))
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

# test <- simError(tot_n = 5, IVBA_n = 3, CoeV_tot = 0.5, frcAct = 0.25, CoeV_RBA = 0.05, sampmax = 50)
# test2 <- simError(simChoice = "contaminant", tot_n = 5, IVBA_n = 3, CoeV_tot = 0.5, CoeV_RBA = 0.05, minFrcAct = .1, maxFrcAct = .5, numbins = 10)
# test3 <- simError(tot_n = 5, IVBA_n = 3, compositeTF = T, Xaggr = 3, 
#                   CoeV_tot = 0.5, frcAct = 0.25, CoeV_RBA = 0.05, 
#                   sampmax = 50, useMeanTot = F, error_tot = T, ivba_model = T)