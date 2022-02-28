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
  
  CoeV_tot = NULL,     # assume coefficient of variation for total metal conc.
  
  RBAmean = 60,     # assume RBA value
  CoeV_RBA = NULL,    # assume coefficient of variation for RBA
  
  ##### SITE-WIDE INPUTS
  sim_site = TRUE,       # simulate sites?
  siteDU_n = NULL,       # number of DU to simulate in each site
  
  site_CoeV_tot = NULL, # coefficient of variation of total concentration among site DUs
  site_Coev_RBA = NULL, # coefficitien of variation of RBA among site DUs
  
  ###### SIMULATION PARAMETERS
  iter = 1000,       # nr. simulations
  ncel = 1000,       # nr. cells in the decision unit
  sampmax = 50       # maximum number of samples to simulate
  
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
  
  tru_mu_rba <- RBAmean
  tru_mu_tot <- ((frcAct*actLvl) + actLvl)/(tru_mu_rba/100)
  
  if(IVBA_n > tot_n) stop("more IVBA samples than total samples")
  
  # relative sdv of RBA and total metal (based on what measurements?)
  cv <- function(x){sd(x)/mean(x)} # function to calculate coefficient of variance
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
  
  # vector of possible sample numbers, starting from user-supplied value
  Ysamp <- c(tot_n:sampmax)   # range of possible number of samples to simulate
  
  # initialize output matrices 
  mx_errYN <- array(rep(NA, length(Ysamp)*iter), dim=c(length(Ysamp),iter),
                    dimnames = list(Ysamp,
                                    paste("i=",1:iter,sep=""))) # matrix of hit(Yes error)/misses(No error)
  
  prd_ba <- mx_errYN # predicted bioavailable metal (mg/kg) for the decision unit
  
  # generate plausible data
  tru_rba <- rnorm(n=ncel, mean=tru_mu_rba,
                        sd=tru_mu_rba*CoeV_RBA) 
  tru_tot <- rnorm(n=ncel, mean=tru_mu_tot,
                       sd=tru_mu_tot*CoeV_tot)
  cells <- cbind(tru_tot,tru_rba)
  
  # empty dataframes for data checking
  samp.DUsample = NULL
  samp.meas_tot = NULL
  samp.meas_ivb = NULL
  samp.prd_ba = NULL
  
  # I) Create realistic true total metal and RBA 
  #    [i.e. populate synthetic decision unit]

  # estimate bioavailable metal and remediation error
  for (j in 1:length(Ysamp)){ # varying amount of samples 
    print(paste("Simulating samples=",Ysamp[j]))
    
    # II) create Ysamp[j] aggregate(i.e. composite) or discrete measurement inputs 
    #     For aggregated cells (Xaggr>1), mix the content of Xaggr cells for each sample
    for (k in 1:iter){
      
      # if number of aggregated samples > 1, then mix samples of cells for measurement input
      # else, treat each sample as a separate measurement input
      if(compositeTF){ # IF COMPOSITING
        measurement_input <- NA*cells[1:Ysamp[j],]
        for (i in 1:Ysamp[j]){
          sel_cel <- sample(1:ncel,Xaggr) # select Xaggr cells
          measurement_input[i,] <- cells[sel_cel,] %>% colMeans() 
          # mix their properties to create Ysamp[j] new cells
        } # these are now our Ysamp[j] random cells
      }else{ # IF DISCRETE
        measurement_input = cells[sample(1:ncel,Ysamp[j]),]
      }
      
      # III) (lab) measure total metal for each input, 
      #      1 time per cell
      meas_tot <- rtruncnorm(n=Ysamp[j], a=0, b=Inf,
                            mean=measurement_input[,"tru_tot"],
                            measurement_input[,"tru_tot"]*5/100)
      
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
      
      # exceeding threshold in this simulation, Y/N?
      if (frcAct>0){
        mx_errYN[j,k] <- as.numeric(ba_DU<actLvl) # for t1 error
      }else{
        mx_errYN[j,k] <- as.numeric(ba_DU>actLvl) # for t2 error
      }
      
      # store bioavailable metal for DU for this simulation
      prd_ba[j,k] <- ba_DU
      
      # output for error checking:
      if(j==1 | j == length(Ysamp)){
        samp.DUsample = 
          bind_rows(samp.DUsample, 
                    data.frame(iteration = k, samples = Ysamp[j], measurement_input) %>% 
                      mutate(tru_rba = replace(tru_rba, -(1:IVBAsamp), NA)) # replace all not sampled with NA
                    )
        samp.meas_tot = bind_rows(samp.meas_tot, data.frame(iteration = k, samples = Ysamp[j], meas_tot))
        samp.meas_ivb = bind_rows(samp.meas_ivb, data.frame(iteration = k, samples = Ysamp[j], meas_ivb))
        samp.prd_ba = bind_rows(samp.prd_ba, data.frame(iteration = k, samples = Ysamp[j], ba_DU))
      }
      
    } # loop over sim
  } # ... over nr samples
  
  err_pb <- data.frame(samples = as.numeric(dimnames(mx_errYN)[[1]]),
                       probability =apply(mx_errYN, 1, mean)*100 # transform Y/N into prob of type I or II errors
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
                                    Hetvals = c(CoeV_tot = CoeV_tot, CoeV_RBA = CoeV_RBA),
                                    iter = iter, 
                                    ncel = ncel, 
                                    sampmax = sampmax,
                                    simWarnings = simWarnings,
                                    samp.DUsample = samp.DUsample,
                                    samp.meas_tot = samp.meas_tot, 
                                    samp.meas_ivb = samp.meas_ivb,
                                    samp.prd_ba = samp.prd_ba)))
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
    paste("Based on the sampling protocol and decision unit assumptions entered, the probability of making a ",
          errortype, " error is <b>", round(err_pb[1,"probability"], 1), "%</b>.",
          sep = "")
  )
}


#####Output plot#####
# Plot the error curve results by number of samples with red line for error threshold

simPlot <- function(simResult){
  
  # Extract simulation variables
  frcAct = simResult$frcAct
  err_pb = simResult$err_pb
  AsPb = simResult$sim_attributes$AsPb
  errortype = simResult$errortype
  
  ggplot(err_pb, aes(x = samples, y = probability)) + geom_line() + 
    xlab("Number of samples") + 
    ylab(paste("Probability of", errortype, "error", sep = " ")) +
    geom_hline(yintercept = ifelse(errortype == "type 1", 5, 20), color = "red") + 
    ggtitle(paste("Change in probability of ",errortype, " error when true bioavailable ", AsPb, " is ",
                  round(abs(frcAct*100),1), "% ", 
                  ifelse(frcAct>0, "above", "below"), " the action level, \nper increase in samples analyzed for total metal and IVBA values",
                  sep = ""),)
}



