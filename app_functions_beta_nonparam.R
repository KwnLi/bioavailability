library(ggplot2)
library(truncnorm) 
library(dplyr)


#####Simulate error function (non-parametric) #####
simError_nonparam <- function(

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
  
  ##### INPUTS FOR ORIGINAL SAMPLING PROTOCOL
  # fraction above/below
  tot_n = NULL,  # how many total metal concentration samples?
  IVBA_n = NULL, # how many samples analyzed for IVBA?
  
  ###### SIMULATION PARAMETERS
  tot_data = NULL,      # measured total concentration data to resample
  rba_data = NULL,      # measured rba data to resample
  iter = 1000,          # nr. simulations
  ncel = 1000,          # nr. cells in the decision unit
  sampmax = NULL        # maximum number of samples to simulate (simChoice = "sample")
){
  
  # define inverse model to calculate observed IVBA from true RBA 
  fy <- function(y, m, b) {
    x = (y - rnorm(1,0,sepred) - b)/m
    return(max(0,x))}
  
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
  cv <- function(x){sd(x)/mean(x)}
  
  # calculate heterogeneity from input data (for info purposes)
  CoeV_RBA <- rba_data %>% fx(m=m, b=b) %>% cv()
  CoeV_tot <- tot_data %>% cv()
  
  # Only varying sample nr.
  sampstart <- ifelse(tot_n > IVBA_n, tot_n, IVBA_n)
  Ysamp <- c(sampstart:(sampstart+sampmax)) # vector of possible sample numbers, starting from user-supplied value
  sim.num <- length(Ysamp)        # parameter for creating output matrix
  dim.x <- Ysamp                  # parameter for creating output matrix
  
  # initialize output matrices 
  mx_errYN <- array(rep(NA, sim.num*iter),
                    dim=c(sim.num, iter),
                    dimnames = list(dim.x, paste("i=",1:iter,sep=""))) # matrix of hit(Yes error)/misses(No error)
  
  prd_ba <- mx_errYN # predicted bioavailable metal (mg/kg) for the decision unit
  
  # empty dataframe lists for data checking
  samp.DUsample = NULL
  samp.meas_tot = NULL
  samp.meas_ivb = NULL
  samp.prd_ba = NULL
  samp.mn = NULL
  
  ### SIMULATIONS ###
 
  # I) Create realistic true total metal and RBA based on fraction above action level
  #    [i.e. populate synthetic decision unit]
  tru_tot <- sample(tot_data, size = ncel, replace = TRUE)
  tru_rba <- sample(rba_data, size = ncel, replace = TRUE)
  cells <- cbind(tru_tot, tru_rba)
  cells_rba <- mean(cells[,"tru_tot"]) * (mean(cells[,"tru_rba"])/100)
  
  # estimate bioavailable metal and remediation error
  for (j in 1:length(Ysamp)){ # varying amount of samples 
    print(paste("Simulating samples =",Ysamp[j]))
    
    # II) create Ysamp[j] aggregate(i.e. composite) or discrete measurement inputs 
    #     For aggregated cells (Xaggr>1), mix the content of Xaggr cells for each sample
    for (k in 1:iter){
      
      # if number of aggregated samples > 1, then mix samples of cells for measurement input
      # else, treat each sample as a separate measurement input
      if(compositeTF){ # IF COMPOSITING
        measurement_input <- NA*cells[1:Ysamp[j],]
        for (z in 1:Ysamp[j]){
          sel_cel <- sample(1:ncel,Xaggr) # select Xaggr cells
          measurement_input[z,] <- cells[sel_cel,] %>% colMeans() 
          # mix their properties to create Ysamp[j] new cells
        } # these are now our Ysamp[j] random cells
      }else{ # IF DISCRETE
        measurement_input = cells[sample(1:ncel,Ysamp[j]),]
      }
      
      # III) (lab) measure total metal for each input, 
      #      1 time per cell
      totsamp <- tot_n + j - 1 # increase tot_n by 1 each sample increase
      meas_tot <- rtruncnorm(n=Ysamp[j], a=0, b=Inf,
                             mean=measurement_input[,"tru_tot"],
                             measurement_input[,"tru_tot"]*5/100)[1:totsamp]
      
      # measure IVBA for [IVBAsamp] measurement_input (i.e. run inverse model), 
      # 1 time per cell
      IVBAsamp <- IVBA_n + j - 1  # increase IVBA by 1 each sample increase
      meas_ivb <- sapply(measurement_input[1:IVBAsamp,"tru_rba"], fy, m=m, b=b)  # convert RBA to IVBA
      
      # upper 95% conf. int. of the mean function:
      upper95 <- function(x,lvl){
        xbar = mean(x)
        up95 = xbar + qnorm(lvl)*(sd(x)/sqrt(length(x)))
        return(up95)
      }
      
      # IV, V) avg IVBA measurements, calculate DU RBA
      if(useMeanIVBA){   # if user wants to use the mean value
        est_rba_DU <- meas_ivb %>% mean() %>% fx(m=m, b=b)
      }else{         # if user wants to use the 95% inverval value of IVBA measurements
        est_rba_DU <- meas_ivb %>% upper95(lvl=.975) %>% fx(m=m, b=b)
      }
      
      # VI) calc bioaval total contaminant mass fraction (mg/kg) for DU 
      if(useMeanTot){   # if user wants to use the mean value
        ba_DU <- mean(meas_tot)*est_rba_DU/100
      }else{         # if user wants to use the 95% inverval value of total
        ba_DU <- upper95(meas_tot, lvl=.975)*est_rba_DU/100
      }
      
      # VII) Outputs
      
      # exceeding threshold in this simulation, Y/N?
      if (cells_rba>actLvl){
        mx_errYN[j,k] <- as.numeric(ba_DU<actLvl) # for t1 error
      }else{
        mx_errYN[j,k] <- as.numeric(ba_DU>actLvl) # for t2 error
      }
      
      # store bioavailable metal for DU for this simulation
      prd_ba[j,k] <- ba_DU
      
      # output for error checking:
      if(j==1 | j == sim.num){
        samp.DUsample = 
          bind_rows(samp.DUsample, 
                    data.frame(iteration = k, n_tot = totsamp, n_rba = IVBAsamp, DU_rba_mn = cells_rba, measurement_input) %>% 
                      mutate(tru_rba = replace(tru_rba, -(1:IVBAsamp), NA)) %>% # replace all not sampled with NA
                      mutate(tru_tot = replace(tru_tot, -(1:totsamp), NA))       # replace all not sampled with NA
          )
        samp.meas_tot = bind_rows(samp.meas_tot, data.frame(iteration = k, n_tot = totsamp, n_rba = IVBAsamp, meas_tot))
        samp.meas_ivb = bind_rows(samp.meas_ivb, data.frame(iteration = k, n_tot = totsamp, n_rba = IVBAsamp, meas_ivb))
        samp.prd_ba = bind_rows(samp.prd_ba, data.frame(iteration = k, n_tot = totsamp, n_rba = IVBAsamp, DU_rba_mn = cells_rba, ba_DU))
      }
      if(j==1){
        samp.mn = bind_rows(samp.mn,
                            data.frame(iteration = k, 
                                       mn_tot = mean(measurement_input[,"tru_tot"]),
                                       mn_rba = mean(measurement_input[,"tru_rba"])
                            )
        )
      }
      
    } # loop over sim
  } # ... over nr samples
  
  
  err_pb <- data.frame(simparam = as.numeric(dimnames(mx_errYN)[[1]]),
                       probability =apply(mx_errYN, 1, mean)*100 # transform Y/N into prob of type I or II errors
  )
  
  return(list(errortype = ifelse(cells_rba>actLvl, "type 1", "type 2"),
              err_pb = err_pb,
              tot_n = tot_n,
              IVBA_n = IVBA_n,
              sim_attributes = list(AsPb = AsPb,
                                    actLvl = actLvl,
                                    compositeTF = compositeTF,
                                    Xaggr = Xaggr, 
                                    useMeanTot = useMeanTot, 
                                    useMeanIVBA = useMeanIVBA,
                                    Hetvals = c(CoeV_tot = CoeV_tot, CoeV_RBA = CoeV_RBA),
                                    iter = iter, 
                                    ncel = ncel, 
                                    sampmax = sampmax,
                                    simWarnings = simWarnings,
                                    cells_rba = cells_rba,
                                    samp.mn = samp.mn,
                                    samp.DUsample = samp.DUsample,
                                    samp.meas_tot = samp.meas_tot, 
                                    samp.meas_ivb = samp.meas_ivb,
                                    samp.prd_ba = samp.prd_ba)))
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
  cells_rba = simResult$sim_attributes$cells_rba
  actLvl = simResult$sim_attributes$actLvl
  actLvl = simResult$sim_attributes$actLvl
  err_pb = simResult$err_pb
  errortype = simResult$errortype
  
  # Output text
  return(
    paste("Based on the sampling protocol and selected decision unit assumptions, with an actual bioavailable contaminant level of ",
          round(cells_rba, 1), " ppm and an action level of ", round(actLvl, 1), 
          " ppm, the probability of making a ",
          errortype, " error is <b>", round(err_pb[1,"probability"], 1), "%</b>.",
          sep = "")
  )
}


#####Output plot#####
# Plot the error curve results by number of samples with red line for error threshold

simPlot <- function(simResult){
  
  # Extract simulation variables
  cells_rba = simResult$sim_attributes$cells_rba
  tot_n = simResult$tot_n
  IVBA_n = simResult$IVBA_n
  err_pb = simResult$err_pb
  AsPb = simResult$sim_attributes$AsPb
  errortype = simResult$errortype
  
  xoffset <- ifelse(tot_n > IVBA_n, tot_n, IVBA_n)
  
  ggplot(err_pb, aes(x = simparam - xoffset, y = probability)) + geom_line() + geom_point() + 
    xlab("Number of additional samples analyzed for both total concentration and IVBA") + 
    ylab(paste("Probability of", errortype, "error", sep = " ")) +
    geom_hline(yintercept = ifelse(errortype == "type 1", 5, 20), color = "red") + 
    ggtitle(paste("Change in probability of ",errortype, 
                  " error with increase in samples analyzed for", 
                  "\ntotal metal and IVBA when true bioavailable ", AsPb, " is ",
                  round(cells_rba,1), " ppm", 
                  sep = ""),) + 
    theme_bw()
}

#####Precision plot######
# Plot distribution of bioavailability predictions and 95% interval

precPlot <- function(simResult, plot.median = FALSE){
  
  # Extract simulation variables
  iter = simResult$sim_attributes$iter
  ba = simResult$sim_attributes$samp.prd_ba[1:iter,]
  actLvl = simResult$sim_attributes$actLvl
  
  # derived values
  ba_95 = quantile(ba$ba_DU, probs=c(.025,.975))
  ba_mn = mean(ba$ba_DU)
  ba_md = median(ba$ba_DU)
  
  outplot <- ggplot(ba, aes(ba_DU)) + geom_histogram(fill = "cornsilk3") + 
    theme_bw() +
    xlab("Predicted bioavailability") +
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

#####Custom Numeric Input#####
numericInputRow <- function(inputId, label, value = NULL, step = NULL, max = NULL, min = NULL) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "numeric", value = value, 
                 step = step, max = max, min = min,
                 class="input-small"))
}

# test <- simError_nonparam(tot_n = 5, IVBA_n = 3, sampmax = 50, tot_data = ucrtot$totPb, rba_data = ucrivba$rbaPb)
# test2 <- simError_nonparam(AsPb = "As", actLvl = 40, tot_n = 5, IVBA_n = 3, sampmax = 50, tot_data = ucrtot$totPb, rba_data = ucrivba$rbaPb)
