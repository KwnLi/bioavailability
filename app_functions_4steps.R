# package checking and installation
# from: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
# using<-function(...) {
#   libs<-unlist(list(...))
#   req<-unlist(lapply(libs,require,character.only=TRUE))
#   need<-libs[req==FALSE]
#   if(length(need)>0){
#     install.packages(need)
#     lapply(need,require,character.only=TRUE)
#   }
# }

# using("shiny", "shinyjs", "cowplot", "ggplot2", "truncnorm", "dplyr", "ggpattern", "egg")

library(shiny)
library(shinyjs)
library(cowplot)
library(ggplot2)
library(truncnorm)
library(dplyr)
library(ggpattern)
library(egg)

##### Custom lognormal #####
# just a wrapper form the rlnorm that converts the normal mean and sd
# inputs so that the output lognormal samples have the specified mean and sd
# at the lognormal scale

custom_rlnorm <- function(n, m, s){
  location <- log(m^2 / sqrt(s^2 + m^2))
  shape <- sqrt(log(1 + (s^2 / m^2)))
  return(rlnorm(n=n, location, shape))
}

##### upper 95% conf. int. of the mean function #####
upper95 <- function(x,lvl){
  xbar = mean(x)
  up95 = xbar + qnorm(lvl)*(sd(x)/sqrt(length(x)))
  return(up95)
}

##### IVBA/RBA regressions #####

# contains the regression parameters for the As and Pb IVBA functions
contam_params <- list(
  As = c(
    sepred = 19/1.96, # from 95% prediction limit for a single As RBA measurement 
    m = 0.79,         # from https://doi.org/10.1080/15287394.2015.1134038
    b = 3
  ),
  Pb = c(
    sepred = 32/1.96, # from 95% prediction limit for a single Pb RBA measurement 
    m = 0.878,       # from OSWER 9285.7-77 (May 2007)
    b = -2.81
  )
)

# Inverse model to calculate observed IVBA from true RBA 
fy_error <- function(
    y, # "true" RBA
    contaminant, # string, "As" or "Pb"
    sepred = NULL
){
  m = contam_params[[contaminant]]["m"]
  b = contam_params[[contaminant]]["b"]
  sepred = ifelse(is.null(sepred), contam_params[[contaminant]]["sepred"], sepred)
  
  x = (y - rnorm(1,0,sepred) - b)/m
  
  return(max(0,x))
}
fy_error <- Vectorize(fy_error)

# Inverse model to calculate observed IVBA from true RBA without model error
fy <- function(
    y, # "true" RBA
    contaminant # string, "As" or "Pb"
){
  m = contam_params[[contaminant]]["m"]
  b = contam_params[[contaminant]]["b"]
  
  x = (y - b)/m
  
  return(max(0,x))
}
fy <- Vectorize(fy)

# Direct model, from mean measured IVBA to estimated RBA
fx <- function(
    x, # measured IVBA
    contaminant # string, "As" or "Pb"
){
  m = contam_params[[contaminant]]["m"]
  b = contam_params[[contaminant]]["b"]
  
  y = m*x+b
  
  return(y)
}
fx <- Vectorize(fx)

##### Simulate DU #####

# distribution parameters
distParam <- list(
  As = list(
    coeV.rba = c(mn = 1.118799762, lvl95 = 0.452062439),
    coeV.tot = c(mn = 0.452062439, lvl95 = 1.118799762)
  ),
  Pb = list(
    coeV.rba = c(mn = 0.55, lvl95 = 0.95),
    coeV.tot = c(mn = 1.12, lvl95 = 1.52)
  )
)

# Function to generate "true" total conc.
simDist_tot <- function(
    n.totmeas,
    tru_mu_tot,
    coeV_tot,
    dist_tot
){
  if(dist_tot == "normal"){
    rnorm(n=n.totmeas, mean=tru_mu_tot, sd=tru_mu_tot*coeV_tot)
  } else if(dist_tot == "lognorm"){
    custom_rlnorm(n=n.totmeas, m=tru_mu_tot, s=tru_mu_tot*coeV_tot)
  } else{
    stop(paste("Unrecognized distribution for total concentration: ",
               dist_tot, sep = ""))
  }
  
}

# Function to generate "true" rba
simDist_rba <- function(
    n.rbameas,
    tru_mu_rba,
    coeV_rba,
    dist_rba
){
  if(dist_rba == "normal"){
    rnorm(n=n.rbameas, mean=tru_mu_rba, sd=tru_mu_rba*coeV_rba)
  } else if(dist_rba == "lognorm"){
    custom_rlnorm(n=n.rbameas, m=tru_mu_rba, s=tru_mu_rba*coeV_rba)
  } else if(dist_rba == "uniform"){
    runif(n=n.rbameas, min = 0, max = 1)
  } else{
    stop(paste("Unrecognized distribution for rba: ",
               dist_rba, sep = ""))
  }
}

##### Simulate samples #####
take_samples <- function(
    n.samp,
    n.incr,
    n.sim,
    distfun,
    ...
){
  n.cores <- n.sim*n.samp*n.incr
  simvalues <- do.call(distfun, args = append(n.cores, list(...)))
  
  # long output
  out.long <- data.frame(
    sim.num = rep(seq(1:n.sim), each = n.samp*n.incr),
    sample.num = rep(rep(seq(1:n.samp), each = n.incr), n.sim),
    incr.num = rep(rep(seq(1:n.incr), times = n.samp)),
    sim.value = simvalues
  )
  
  return(out.long)
}

# test <- take_samples(5, 2, 20, "simDist_rba", 50, 0.3, "lognorm")

##### Measurement error #####
apply_meas_error <- function(
    tru.val = NULL,
    max.val = Inf,
    coefVar = 0.05
){
  meas.tot <- rtruncnorm(n=length(tru.val), a=0, b=max.val,
                         mean = tru.val,
                         sd = mean(tru.val)*coefVar)
  return(meas.tot)
}

##### Simulate DU function #####
simDU <- function(
  AsPb = NULL,        # "As" or "Pb"
  
  # What is the site-specific soil contaminant action level?
  actLvl = 400,       # mg/kg  (action level: limit of biovalability above which site has to be remedied)
  actLvlRBA = 60,     # % RBA assumed in the action level
  frcAct = NULL,    # fraction of the action level threshold
  
  # Use mean of upper 95% interval in estimation of total contaminant?
  useMeanTot = T,       # T = use the mean; F = use 95% interval
  
  # Use mean or upper 95% interval in estimation of IVBA?
  useMeanIVBA = T,      # T = use the mean; F = use 95% interval
  
  # sampling protocol inputs
  tot.n = NULL,   # how many total metal concentration samples?
  tot.incr = 1, # number of increments in total samples
  
  ivba.n = NULL,  # how many samples analyzed for IVBA?
  ivba.incr = 1,# number of increments in ivba samples
  
  # simulated DU parameters
  coeV.tot = NULL,
  coeV.rba = NULL,
  mn.rba = 60,
  dist_tot = "lognorm", # distribution of total concentration
  dist_rba = "normal",  # distribution of RBA
  
  # simulation parameters
  custom_sepred = NULL, # custom sepred value, overrides defaults
  error_tot = FALSE,    # include total conc. measurement error?
  error_ivb = FALSE,    # include IVBA measurement error?
  error_ivb_cv = NULL,  # IVBA measurement error coefficient of variation
  ivba_model = FALSE,   # include IVBA model error?
  post_mean = FALSE,    # calculate IVBA model error after summarizing across samples?
  iter = 1000,          # nr. simulations
  
  # Output detail
  outputLvl = 1         # 1=just error rate and summary across sims
                        # 2=level1 + summary of each simulation
                        # 3=level2 + total and ivb samples for each iteration
                        # 4=level3 + uncomposited increments for each iteration
){
  # check that AsPb is defined
  if(is.null(AsPb)){stop("Contaminant is not defined for DU sim")}
  
  # Adjusted action level
  actLvl.adj <- actLvl * (actLvlRBA/100)
  
  # True total conc.
  tru_ba <- (frcAct*actLvl.adj) + actLvl.adj
  tru_mu_tot <- tru_ba/(mn.rba/100)
  
  # True ivba
  tru_mu_ivb <- fy(mn.rba, contaminant = AsPb)
  
  # simulate the DU
  tot.sim.incr <- take_samples(tot.n, tot.incr, iter, 
                             "simDist_tot", tru_mu_tot=tru_mu_tot, 
                             coeV_tot=coeV.tot, dist_tot=dist_tot)
  rba.sim.incr <- take_samples(ivba.n, ivba.incr, iter, 
                             "simDist_rba", tru_mu_rba=mn.rba, 
                             coeV_rba=coeV.rba, dist_rba=dist_rba)
  
  tot.sim.meas <- tot.sim.incr %>% group_by(sim.num, sample.num) %>%
    summarize(tru.tot = mean(sim.value), .groups = "drop") %>% # take composite
    mutate(meas.tot = apply_meas_error(tru.tot)) # apply measurement error
  
  rba.sim.meas <- rba.sim.incr %>% group_by(sim.num, sample.num) %>%
    summarize(tru.rba = mean(sim.value), .groups = "drop") %>% # take composite
    mutate(tru.ivb = if(ivba_model & !post_mean){ # TRUE/FALSE if modeling ivba BEFORE taking mean
      fy_error(tru.rba, contaminant = AsPb)
    }else{
      fy(tru.rba, contaminant = AsPb)
    }
    ) %>%
    mutate(meas.ivb = if(error_ivb){  # TRUE/FALSE apply IVBA measurement error?
      apply_meas_error(tru.ivb, max.val=100, coefVar=error_ivb_cv)
    }else{
      tru.ivb
    }
    )
  
  # take mean/95% UL of DU samples for each iteration
  DU.sim <- tot.sim.meas %>% group_by(sim.num) %>%
    summarize(
      n_tot = n(),
      est_tot_DU = if(useMeanTot){ # TRUE/FALSE using mean of total conc. vals
      mean(meas.tot)  # if true
        }else{
          upper95(meas.tot, lvl=.975)
          }, .groups = "drop") %>%
    left_join(  # join the rba data
      rba.sim.meas %>% group_by(sim.num) %>%
        summarize(
          n_rba = n(),
          est_ivb_DU = if(useMeanIVBA){ # TRUE/FALSE using mean IVBA
            mean(meas.ivb)  # if true
            }else{
              upper95(meas.ivb, lvl=.975)
              }, .groups = "drop") %>%
        mutate(est_rba_DU = if(ivba_model & post_mean){  # TRUE/FALSE if modeling ivba AFTER taking mean
          fx(est_ivb_DU, contaminant = AsPb) %>% # convert to rba 
            fy_error(contaminant = AsPb) %>%     # convert to ivba with model error
            fx(contaminant = AsPb)               # convert to rba again
        }else{
          fx(est_ivb_DU, contaminant = AsPb)     # just convert to rba (model error previously applied or not at all)
        }),
      by = "sim.num"
      ) %>%
    mutate(ba_DU = est_tot_DU*est_rba_DU/100) %>%
    mutate(errYN = if(frcAct>=0){
        as.numeric(ba_DU<actLvl.adj)
      }else{
        as.numeric(ba_DU>actLvl.adj)
      })
  
  err_pb <- DU.sim %>% 
    summarize(
      n_tot = mean(n_tot),
      n_rba = mean(n_rba),
      actLvl = actLvl,
      actLvlRBA = actLvlRBA,
      actLvl.adj = actLvl.adj,
      frcAct = frcAct,
      tru_tot = tru_mu_tot,
      tru_ba = tru_ba,
      tru_rba = mn.rba,
      tru_ivb = tru_mu_ivb,
      err_pb = mean(errYN)
      )
  
  return(
  if(outputLvl == 1){
    list(err_pb = err_pb)
  }else if(outputLvl == 2){
    list(err_pb = err_pb, DU_sim = DU.sim)
  }else if(outputLvl == 3){
    list(err_pb = err_pb, DU_sim = DU.sim,
         measurements = list(tot=tot.sim.meas, rba=rba.sim.meas))
  }else if(outputLvl == 4){
    list(err_pb = err_pb, DU_sim = DU.sim,
         samples = list(tot=tot.sim.meas, rba=rba.sim.meas),
         increments = list(tot=tot.sim.incr, rba=rba.sim.incr))
  }
  )
  
}

# test <- simDU(
#   AsPb = "As",
#   frcAct = 0.15,
#   tot.n = 5,
#   ivba.n = 3,
#   coeV.tot = 0.5,
#   coeV.rba = 0.05,
#   error_tot = TRUE,
#   error_ivb = TRUE,
#   error_ivb_cv = 0.05,
#   ivba_model = TRUE,
#   outputLvl = 2
# )

##### Step 1 function #####
# determine sequence of sample sizes to simulate
step1 <- function(
    AsPb = NULL,       # define contaminant
    tot.n = NULL,      # number of total conc. samples, passed to simDU
    ivba.n = NULL,     # number of ivba samples, passed to simDU
    sampmax = NULL,    # maximum number of samples to simulate, default 10*tot.n
    incr.vec = NULL,   # vector of increments to test
    ...                # simDU parameters
){
  sampout <- length(seq(max(c(tot.n, ivba.n)), sampmax, by=1))
  tot.range <- seq(tot.n, by=1, length.out = sampout)
  ivba.range <- seq(ivba.n, by=1, length.out = sampout)
  
  step1.out <- list()
  
  for(h in 1:length(incr.vec)){
    step1.h <- list()
    for(i in 1:sampout){
      # progress message
      print(paste("Simulating", tot.range[i], "total concentration samples and", 
                  ivba.range[i], "IVBA samples, with", incr.vec[h], "increments", sep = " "))
      
      step1.h[[i]] <- simDU(
        AsPb = AsPb,
        tot.n = tot.range[i],
        ivba.n = ivba.range[i],
        outputLvl = 1,
        tot.incr = incr.vec[h],
        ivba.incr = incr.vec[h],
        ...
      )$err_pb
    }
    step1.out[[h]] <- bind_rows(step1.h) %>% mutate(n_incr = incr.vec[h])
  }
  

  return(list(step1 = bind_rows(step1.out), AsPb = AsPb))
}

# test.step1 <- step1(
#   AsPb = "Pb",
#   tot.n = 5,
#   ivba.n = 3,
#   sampmax = 50,
#   frcAct = -0.25,
#   coeV.tot = 0.5,
#   coeV.rba = 0.3,
#   error_tot = TRUE,
#   error_ivb = TRUE,
#   error_ivb_cv = 0.05,
#   ivba_model = TRUE,
#   incr.vec = c(1,5)
# )

##### Step 2 #####
# Vary the contaminant level

step2 <- function(
    AsPb = NULL,       # define contaminant
    tot.n = NULL,      # number of total conc. samples, passed to simDU
    ivba.n = NULL,     # number of ivba samples, passed to simDU
    minFrcAct = NULL,  # minimum fraction of action level to simulate
    maxFrcAct = NULL,  # maximum fraction of action level to simulate
    numbins = 20,      # nr. divisions over range of contaminant levels
    ...                # simDU parameters
){
  if(minFrcAct*maxFrcAct < 0){stop("Min and max fraction of action level range cannot span zero")}
  
  # make sure minFrcAct is less than maxFrcAct
  if(minFrcAct > maxFrcAct){
    tempval <- maxFrcAct
    maxFrcAct <- minFrcAct
    minFrcAct <- tempval
  }
  
  frcActRange <- round(seq(from = ifelse(minFrcAct>-1,minFrcAct,-0.99), 
                                to = ifelse(maxFrcAct>-1,maxFrcAct,-0.01), 
                                length.out = numbins), 2)
  
  err_pb <- list()
  simQt <- list()
  for(i in 1:length(frcActRange)){
    # progress message
    print(paste("Simulating", 100*abs(frcActRange[i]), "%",
                ifelse(frcActRange[i]<0,"below","above"),
                "the action level", sep = " "))
    sim.i <- simDU(
      AsPb = AsPb,
      tot.n = tot.n,
      ivba.n = ivba.n,
      frcAct = frcActRange[i],
      outputLvl = 2,
      ...
    )
    
    err_pb[[i]] <- sim.i$err_pb
    simQt[[i]] <- quantile(sim.i$DU_sim$ba_DU, probs = c(0.05, 0.25, 0.50, 0.75, 0.95))
  }
  
  simQt <- bind_rows(simQt) %>% rename_with(.fn = ~paste0("ba_sim_", .))
  
  step2.out <- bind_rows(err_pb) %>% bind_cols(simQt)
  
  return(list(step2 = step2.out, AsPb = AsPb))
}

# test.step2.2 <- step2(
#   AsPb = "Pb",
#   tot.n = 5,
#   tot.incr = 10,
#   ivba.n = 3,
#   ivba.incr = 10,
#   minFrcAct = -0.01,
#   maxFrcAct = -0.99,
#   coeV.tot = 0.5,
#   coeV.rba = 0.05,
#   error_tot = TRUE,
#   error_ivb = TRUE,
#   error_ivb_cv = 0.05,
#   ivba_model = TRUE
# )

##### Step 3 #####
# Vary the contaminant level

step3 <- function(
    meas.tot = NULL,      # actual total concentration measurements
    meas.ivba = NULL,     # actual ivba measurements
    AsPb = NULL,          # "As" or "Pb"
    
    # Sampling protocol
    tot.incr = NULL,      # number of increments for total conc. samples
    ivba.incr = NULL,     # number of increments for ivb samples
    
    # Use mean of upper 95% interval in estimation of total contaminant?
    useMeanTot = T,       # T = use the mean; F = use 95% interval
    
    # What is the site-specific soil contaminant action level?
    actLvl = 400,       # mg/kg  (action level: limit of biovalability above which site has to be remedied)
    actLvlRBA = 60      # % RBA assumed in the action level
){
  
  meas.rba <- fx(meas.ivba, contaminant = AsPb)
    
  tot.n <- length(meas.tot)
  rba.n <- length(meas.rba)
  
  mn.tot <- ifelse(useMeanTot, mean(meas.tot), upper95(meas.tot))
  mn.rba <- mean(meas.rba)
  
  # coefficient of variation
  coeV.tot <- (sd(meas.tot)*sqrt(tot.incr))/mn.tot
  coeV.rba <- (sd(meas.rba)*sqrt(ivba.incr))/mn.rba
  
  # Adjusted action level
  actLvl.adj <- actLvl * (actLvlRBA/100)
  
  # True total conc.
  meas.ba <- mn.tot*(mn.rba/100)
  meas.frcAct <- (meas.ba - actLvl.adj)/actLvl.adj
  
  step3.out <- list(
    meas.frcAct = meas.frcAct,
    meas.ba = meas.ba,
    actLvl.adj = actLvl.adj,
    mn.tot = mn.tot,
    tot.n = tot.n,
    coeV.tot = coeV.tot,
    mn.rba = mn.rba,
    rba.n = rba.n,
    coeV.rba = coeV.rba,
    sd.tot = sd(meas.tot),
    sd.rba = sd(meas.rba)
  )

  return(list(step3 = step3.out))
}

# teststep3 = step3(c(340,580,209,300,333), meas.ivba = c(78,76,45),
#       tot.incr = 5, ivba.incr = 5,
#       "Pb")

# test the coev back calculation

testcv <- function(x, incr){    # function returns incremented x by taking means in groups of 'incr'
  x.mat <- matrix(x, ncol = incr)
  return(rowMeans(x.mat))
}

# teststep3 <-  step3(testcv(rnorm(n = 10000, mean = 300, sd = 20),100),
#       meas.ivba = testcv(rnorm(n = 10000, mean = 75, sd = 5),100),
#       tot.incr = 100, ivba.incr = 100, "Pb") # simulates increments of 10

# as effective number of "cores" increases (i.e., base unit of sampling whether composited or not), e
# estimated coeV gets closer to the original inputted coeV (sd=20, mn = 300)

##### Step 4 #####
# Calculate precision and accuracy

step4 <- function(
    meas.tot = NULL,      # actual total concentration measurements
    meas.ivba = NULL,     # actual ivba measurements
    AsPb = NULL,          # "As" or "Pb"
    
    # Sampling protocol
    tot.incr = NULL,      # number of increments for total conc. samples
    ivba.incr = NULL,     # number of increments for ivb samples
    
    # Use mean of upper 95% interval in estimation of total contaminant?
    useMeanTot = T,       # T = use the mean; F = use 95% interval
    
    # What is the site-specific soil contaminant action level?
    actLvl = 400,       # mg/kg  (action level: limit of biovalability above which site has to be remedied)
    actLvlRBA = 60,     # % RBA assumed in the action level
    
    ... # simDU inputs
){
  
  meas.dist.param <- step3(meas.tot = meas.tot,
                           meas.ivba = meas.ivba,
                           AsPb = AsPb,
                           tot.incr = tot.incr,
                           ivba.incr = ivba.incr,
                           actLvl = actLvl,
                           actLvlRBA = actLvlRBA,
                           useMeanTot = useMeanTot)
  
  accuracy.sim <- simDU(
    AsPb = AsPb, actLvl = actLvl, actLvlRBA = actLvlRBA,
    frcAct = 0, 
    tot.n = meas.dist.param$step3$tot.n, tot.incr = tot.incr,
    ivba.n = meas.dist.param$step3$rba.n, ivba.incr = ivba.incr,
    coeV.tot = meas.dist.param$step3$coeV.tot,
    coeV.rba = meas.dist.param$step3$coeV.rba,
    mn.rba = meas.dist.param$step3$mn.rba,
    useMeanTot = useMeanTot,
    outputLvl = 2,
    ...
  )
  
  precision.sim <- simDU(
    AsPb = AsPb, actLvl = actLvl, actLvlRBA = actLvlRBA,
    frcAct = meas.dist.param$step3$meas.frcAct, 
    tot.n = meas.dist.param$step3$tot.n, tot.incr = tot.incr,
    ivba.n = meas.dist.param$step3$rba.n, ivba.incr = ivba.incr,
    coeV.tot = meas.dist.param$step3$coeV.tot,
    coeV.rba = meas.dist.param$step3$coeV.rba,
    mn.rba = meas.dist.param$step3$mn.rba,
    useMeanTot = useMeanTot,
    outputLvl = 2,
    ...
  )
  
  return(list(accuracy.sim = accuracy.sim, precision.sim = precision.sim, step3 = meas.dist.param$step3))
}

# teststep4 <- step4(testcv(rnorm(n = 100, mean = 400, sd = 39),10),
#                    meas.ivba = testcv(rnorm(n = 100, mean = 78, sd = 15),10),
#       tot.incr = 10, ivba.incr = 10, "Pb")
