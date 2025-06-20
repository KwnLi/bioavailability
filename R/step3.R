#' Step 3
#'
#' Calculate actual DU statistics
#'
#' @param meas.tot actual total concentration measurements
#' @param meas.ivba actual ivba measurements
#' @param AsPb "As" or "Pb"
#' @param tot.incr number of increments for total conc. samples
#' @param ivba.incr number of increments for ivb samples
#' @param useMeanTot Use mean of upper 95% interval in estimation of total
#'   contaminant? T = use the mean; F = use 95% interval
#' @param actLvl What is the site-specific soil contaminant action level? mg/kg
#'   (action level: limit of biovalability above which site has to be remedied)
#' @param actLvlRBA % RBA assumed in the action level
#'
#' @returns simulation results
#' @export
#'
#' @examples
#' teststep3 = step3(c(340,580,209,300,333), meas.ivba = c(78,76,45),
#'       tot.incr = 5, ivba.incr = 5,
#'       "Pb")
#' teststep3 <-  step3(testcv(rnorm(n = 10000, mean = 300, sd = 20),100),
#'       meas.ivba = testcv(rnorm(n = 10000, mean = 75, sd = 5),100),
#'       tot.incr = 100, ivba.incr = 100, "Pb") # simulates increments of 10
step3 <- function(meas.tot = NULL, meas.ivba = NULL, AsPb = NULL,
                  tot.incr = NULL, ivba.incr = NULL, useMeanTot = T,
                  actLvl = 400, actLvlRBA = 60
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
