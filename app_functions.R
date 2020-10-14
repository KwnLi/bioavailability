library(ggplot2)
library(truncnorm) 
library(dplyr)

#####Simulate Pb error function#####
simError <- function(
  # Are you modeling Pb or As?
  AsPb = "Pb",        # "As" or "Pb"
  
  # What is the site-specific soil Pb action level?
  actLvl = 400,       # mg/kg  (action level: limit of biovalability above which site has to be remedied)
  
  # Enter the total soil [Pb] (in ppm) for each of the X samples:
  tot = c(800,1000,790,709,1120),
  
  # Enter the % IVBA results for the Y samples
  IVBA_meas = c(61,65,70),
  
  # Were the samples composited?
  compositeTF = F,
  
  # If yes, how many increments were combined per composite?
  Xaggr = 1,         # composite cells to aggregate
  
  # Use mean of upper 95% interval in estimation of total Pb?
  useMean = T,       # T = use the mean of inputs; F = use 95% interval of inputs
  
  # Should the heterogeneity of the input samples be used in simulation, or default values?
  heterogeneity = "default",  # "default" = use default heterogeneity
                              # "sample" = use the sample hetergeneity
  
  # Should heterogeneity simulation be based on mean or 95% upper level?
  useHetMnTF = TRUE,        # TRUE = use mean heterogeneity; FALSE = use upper 95%
  
  #Simulation parameters
  iter = 1000,       # nr. simulations
  ncel = 1000,       # nr. cells in the decision unit
  sampmax = 50       # maximum number of samples to simulate
){
  # define simulation parameters 
  Ysamp <- c(length(tot):sampmax)   # range of possible number of samples to simulate
  
  if(AsPb == "As"){
    sepred = 19/1.96 # from 95% prediction limit for a single As RBA measurement 
    
    # define inverse model to calculate observed IVBA from true RBA 
    fy <- function(y) { # from https://doi.org/10.1080/15287394.2015.1134038
      x = (y - rnorm(1,0,sepred) - 3)/.79
      return(max(0,x))}
    
    # define direct model, from mean measured IVBA to estimated RBA
    fx <- function(x) { 
      y = .79*x+3
      return(y)}
  }else if(AsPb == "Pb"){
    sepred = 32/1.96 # from 95% prediction limit for a single Pb RBA measurement 
    
    # define inverse model to calculate observed IVBA from true RBA 
    fy <- function(y) { # from OSWER 9285.7-77 (May 2007)
      x = (y - rnorm(1,0,sepred) - -2.81)/0.8782
      return(max(0,x))}
    
    # define direct model, from mean measured IVBA to estimated RBA
    fx <- function(x) { y = 0.8782*x + -2.81
    return(y)}
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
  
  # % bioavailable Pb (RBA) based on model and mean of IVBA inputs
  tru_mu_rba <- fx(mean(IVBA_meas)) 
  
  # mean total Pb (mg/kg)
  tru_mu_tot  <- mean(tot) 
  
  # fraction increase in mean bioavailable Pb (mg/kg) above the action level
  if(useMean){   # if user wants to use the mean value
    frc_incr_abo_thr <- (tru_mu_rba/100*tru_mu_tot - actLvl)/actLvl
  }else{         # if user wants to use the 95% inverval value of total Pb
    frc_incr_abo_thr <- (tru_mu_rba/100*quantile(tot,.95) - actLvl)/actLvl
  }
  
  # relative sdv of RBA and Pb (based on what measurements?)
  cv <- function(x){sd(x)/mean(x)} # function to calculate coefficient of variance
  if(heterogeneity=="default"){  # if using default mean heterogeneity
    if(AsPb == "Pb"){
      CoeV_RBA <- ifelse(useHetMnTF, 0.55, 0.95) # use mean value, else use 95% level
      CoeV_tot <- ifelse(useHetMnTF, 1.12, 1.52)
    } else if(AsPb == "As"){
      CoeV_RBA <- ifelse(useHetMnTF, 1.118799762, 0.452062439)
      CoeV_tot <- ifelse(useHetMnTF, 0.452062439, 1.118799762)
    }
  }else if(heterogeneity=="sample"){  # if calculating from input data
    CoeV_RBA <- IVBA_meas %>% fx() %>% cv()
    CoeV_tot <- tot %>% cv()
  }
  
  # initialize output matrices 
  mx_errYN <- array(rep(NA, length(Ysamp)*iter), dim=c(length(Ysamp),iter),
                    dimnames = list(Ysamp,
                                    paste("i=",1:iter,sep=""))) # matrix of hit(Yes error)/misses(No error)
  
  prd_bPb <- mx_errYN # predicted bioavailable Pb (mg/kg) for the decision unit
  
  # generate plausible data
  tru_rba <- rtruncnorm(n=ncel, a=0, b=Inf, mean=tru_mu_rba,
                        sd=tru_mu_rba*CoeV_RBA) 
  tru_tot <- rtruncnorm(n=ncel, a=0, b=Inf, mean=tru_mu_tot,
                       sd=tru_mu_tot*CoeV_tot)
  cells <- cbind(tru_tot,tru_rba)
  
  # I) Create realistic true Pb and RBA 
  #    [i.e. populate synthetic decision unit]
  
  # estimate bioavailable Pb and remediation error
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
      
      # III) (lab) measure Pb for each input, 
      #      1 time per cell
      meas_Pb <- rtruncnorm(n=Ysamp[j], a=0, b=Inf,
                            mean=measurement_input[,"tru_tot"],
                            measurement_input[,"tru_tot"]*10/100)
      
      # measure IVBA for [IVBAsamp] measurement_input (i.e. run inverse model), 
      # 1 time per cell
      IVBAsamp <- length(IVBA_meas) + j - 1  # increase IVBA by 1 each sample increase
      meas_ivb <- sapply(measurement_input[1:IVBAsamp,"tru_rba"],fy)  # convert RBA to IVBA
      
      # IV, V) avg IVBA measurements, calculate DU RBA
      est_rba_DU <- meas_ivb %>% mean() %>% fx
      
      # VI) calc bioaval Pb for DU 
      if(useMean){   # if user wants to use the mean value
        baPb_DU <- mean(meas_Pb)*est_rba_DU/100
      }else{         # if user wants to use the 95% inverval value of total Pb
        baPb_DU <- quantile(meas_Pb,.95)*est_rba_DU/100
      }
      
      # exceeding threshold in this simulation, Y/N?
      if (frc_incr_abo_thr>0){
        mx_errYN[j,k] <- as.numeric(baPb_DU<actLvl) # for t1 error
      }else{
        mx_errYN[j,k] <- as.numeric(baPb_DU>actLvl) # for t2 error
      }
      
      # store bioaval Pb for DU for this simulation
      prd_bPb[j,k] <- baPb_DU
      
    } # loop over sim
  } # ... over nr samples
  
  err_pb <- data.frame(samples = as.numeric(dimnames(mx_errYN)[[1]]),
                       probability =apply(mx_errYN, 1, mean)*100 # transform Y/N into prob of type I or II errors
  )
  
  return(list(frc_incr_abo_thr = frc_incr_abo_thr, 
              tru_mu_tot = tru_mu_tot, 
              tru_mu_rba = tru_mu_rba, 
              err_pb = err_pb,
              sim_attributes = list(AsPb = AsPb,
                                    actLvl = actLvl, 
                                    compositeTF = compositeTF,
                                    Xaggr = Xaggr, 
                                    useMean = useMean, 
                                    heterogeneity = heterogeneity,
                                    useHetMnTF = useHetMnTF,
                                    Hetvals = c(CoeV_tot = CoeV_tot, CoeV_RBA = CoeV_RBA),
                                    iter = iter, 
                                    ncel = ncel, 
                                    sampmax = sampmax,
                                    simWarnings = simWarnings)))
}


#####Output text#####
simText <- function(simResult){
  
  # Extract simulation variables
  AsPb = simResult$sim_attributes$AsPb
  frc_incr_abo_thr = simResult$frc_incr_abo_thr
  tru_mu_rba = simResult$tru_mu_rba
  tru_mu_tot = simResult$tru_mu_tot
  useMean = simResult$sim_attributes$useMean
  actLvl = simResult$sim_attributes$actLvl
  err_pb = simResult$err_pb
  
  # Determine error type
  errortype <- ifelse(frc_incr_abo_thr>0, "type 2", "type 1")
  
  # Output text
  return(
    paste("Estimated mean bioavailable <b>", AsPb, " = ", round(tru_mu_rba/100*tru_mu_tot, 1),
          "mg/kg</b>. ", ifelse(useMean, "This value", "The 95% interval of this value"),
          " is <b>", round(abs(frc_incr_abo_thr*100), 1), "% ",
          ifelse(frc_incr_abo_thr>0, "above", "below"), " the action level</b> (",
          actLvl, "mg/kg). Based on the sample info provided, the probability that the conclusion <i>'soil ", AsPb, " ",
          ifelse(errortype == "type 1", "does not exceed ", "exceeds "), "the action level'</i> <b>is wrong (i.e. a ",
          errortype, " error has been made) is ", round(err_pb[1,"probability"], 1), 
          "% </b>. EPA has set guidance that the probability of incorrectly concluding that bioavailability of soil ", AsPb, " ",
          ifelse(errortype == "type 1", "exceeds ", "does not exceed "), "the action level should be",
          ifelse(errortype == "type 1", " less than 5%", " less than 20%"), ".",
          sep = "")
  )
}


#####Output plot#####
# Plot the error curve results by number of samples with red line for error threshold

simPlot <- function(simResult){
  
  # Extract simulation variables
  frc_incr_abo_thr = simResult$frc_incr_abo_thr
  err_pb = simResult$err_pb
  AsPb = simResult$sim_attributes$AsPb
  
  # Determine error type
  errortype <- ifelse(frc_incr_abo_thr>0, "type 2", "type 1")
  
  ggplot(err_pb, aes(x = samples, y = probability)) + geom_line() + 
    xlab("Number of samples") + 
    ylab(paste("Probability of", errortype, "error", sep = " ")) +
    geom_hline(yintercept = ifelse(errortype == "type 1", 5, 20), color = "red") + 
    ggtitle(paste("Probability of ",errortype, " error when estimated bioavailable ", AsPb, " is ",
                  round(abs(frc_incr_abo_thr*100),1), "% ", 
                  ifelse(frc_incr_abo_thr>0, "above", "below"), " the action level", sep = ""))
}



