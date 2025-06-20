#' Step 4
#'
#' Calculate precision and accuracy
#'
#' @param meas.tot actual total concentration measurements
#' @param meas.ivba actual ivba measurements
#' @param AsPb "As" or "Pb"
#' @param tot.incr Sampling protocol: number of increments for total conc. samples
#' @param ivba.incr Sampling protocol: number of increments for ivb samples
#' @param useMeanTot Use mean of upper 95% interval in estimation of total contaminant? T = use the mean; F = use 95% interval
#' @param actLvl What is the site-specific soil contaminant action level? Action level: limit of biovalability above which site has to be remedied (mg/kg)
#' @param actLvlRBA % RBA assumed in the action level
#' @param ... `simDU` inputs
#'
#' @return simulation results
#' @export
#'
#' @examples
#' # teststep4 <- step4(testcv(rnorm(n = 100, mean = 400, sd = 39),10), meas.ivba = testcv(rnorm(n = 100, mean = 78, sd = 15),10), tot.incr = 10, ivba.incr = 10, "Pb")
step4 <- function(meas.tot = NULL, meas.ivba = NULL, AsPb = NULL,
                  tot.incr = NULL, ivba.incr = NULL, useMeanTot = T,
                  actLvl = 400, actLvlRBA = 60, ...
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
    outputLvl = 4,
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
    outputLvl = 4,
    ...
  )

  return(list(accuracy.sim = accuracy.sim, precision.sim = precision.sim, step3 = meas.dist.param$step3))
}
