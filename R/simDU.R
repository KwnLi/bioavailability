#' Simulate DU function
#'
#' @param AsPb  "As" or "Pb"
#' @param actLvl  Site-specific soil contaminant action level: mg/kg  (action
#'   level: limit of biovalability above which site has to be remedied)
#' @param actLvlRBA Site-specific soil contaminant action level: % RBA assumed
#'   in the action level
#' @param frcAct Site-specific soil contaminant action level: fraction of the
#'   action level threshold
#' @param useMeanTot Use mean of upper 95% interval in estimation of total
#'   contaminant? T = use the mean; F = use 95% interval
#' @param useMeanIVBA Use mean or upper 95% interval in estimation of IVBA? T =
#'   use the mean; F = use 95% interval
#' @param tot.n sampling protocol input: number of total metal concentration
#'   samples
#' @param tot.incr sampling protocol input: number of increments in total
#'   samples
#' @param ivba.n sampling protocol input: number of samples analyzed for IVBA
#' @param ivba.incr sampling protocol input: number of increments in ivba
#'   samples
#' @param coeV.tot simulated DU parameter: coefficient of variation of total
#'   metal concentration samples
#' @param coeV.rba simulated DU parameter: coefficient of varation of RBA
#' @param mn.rba mean "true" RBA
#' @param dist_tot distribution of total concentration (input parameter for
#'   `simDist_tot`)
#' @param dist_rba distribution of RBA (input parameter for `simDist_rba`)
#' @param custom_sepred simulation parameter: custom sepred value, overrides
#'   defaults
#' @param error_tot simulation parameter: (T/F) include total conc. measurement
#'   error
#' @param error_tot_cv simulation parameter: (T/F) total conc. measurement error
#'   coefficient of variation
#' @param error_ivb simulation parameter: (T/F) include IVBA measurement error
#' @param error_ivb_cv simulation parameter: (T/F) IVBA measurement error
#'   coefficient of variation
#' @param ivba_model simulation parameter: (T/F) include IVBA model error
#' @param post_mean simulation parameter: (T/F) calculate IVBA model error after
#'   summarizing across samples
#' @param iter simulation parameter: number of simulations
#' @param outputLvl Output detail: `1`=just error rate and summary across sims;
#'   `2`=level1 + summary of each simulation; `3`=level2 + total and ivb samples
#'   for each iteration; `4`=level3 + uncomposited increments for each iteration
#'
#' @return
#' @export
#'
#' @examples
#' test <- simDU(AsPb = "As", frcAct = 0.15, tot.n = 5, ivba.n = 3,
#' coeV.tot = 0.5, coeV.rba = 0.05, error_tot = TRUE, error_ivb = TRUE,
#' error_ivb_cv = 0.05, ivba_model = TRUE, outputLvl = 2)
#'
simDU <- function(
    AsPb = NULL,
    actLvl = 400, actLvlRBA = 60, frcAct = NULL,
    useMeanTot = T,
    useMeanIVBA = T,
    tot.n = NULL,
    tot.incr = 1,
    ivba.n = NULL,
    ivba.incr = 1,
    coeV.tot = NULL,
    coeV.rba = NULL,
    mn.rba = 60,
    dist_tot = "lognorm",
    dist_rba = "normal",
    custom_sepred = NULL,
    error_tot = FALSE,
    error_tot_cv = NULL,
    error_ivb = FALSE,
    error_ivb_cv = NULL,
    ivba_model = FALSE,
    post_mean = FALSE,
    iter = 1000,
    outputLvl = 1
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

  tot.sim.meas <- tot.sim.incr |> dplyr::group_by(sim.num, sample.num) |>
    dplyr::summarize(tru.tot = mean(sim.value), .groups = "drop") |> # take composite
    dplyr::mutate(meas.tot = apply_meas_error(tru.tot, coefVar=error_tot_cv)) # apply measurement error

  rba.sim.meas <- rba.sim.incr |> dplyr::group_by(sim.num, sample.num) |>
    dplyr::summarize(tru.rba = mean(sim.value), .groups = "drop") |> # take composite
    dplyr::mutate(tru.ivb = if(ivba_model & !post_mean){ # TRUE/FALSE if modeling ivba BEFORE taking mean
      fy_error(tru.rba, contaminant = AsPb)
    }else{
      fy(tru.rba, contaminant = AsPb)
    }
    ) |>
    dplyr::mutate(meas.ivb = if(error_ivb){  # TRUE/FALSE apply IVBA measurement error?
      apply_meas_error(tru.ivb, max.val=100, coefVar=error_ivb_cv)
    }else{
      tru.ivb
    }
    )

  # take mean/95% UL of DU samples for each iteration
  DU.sim <- tot.sim.meas |> dplyr::group_by(sim.num) |>
    dplyr::summarize(
      n_tot = dplyr::n(),
      est_tot_DU = if(useMeanTot){ # TRUE/FALSE using mean of total conc. vals
        mean(meas.tot)  # if true
      }else{
        upper95(meas.tot, lvl=.975)
      }, .groups = "drop") |>
    dplyr::left_join(  # join the rba data
      rba.sim.meas |> dplyr::group_by(sim.num) |>
        dplyr::summarize(
          n_rba = dplyr::n(),
          est_ivb_DU = if(useMeanIVBA){ # TRUE/FALSE using mean IVBA
            mean(meas.ivb)  # if true
          }else{
            upper95(meas.ivb, lvl=.975)
          }, .groups = "drop") |>
        dplyr::mutate(est_rba_DU = if(ivba_model & post_mean){  # TRUE/FALSE if modeling ivba AFTER taking mean
          fx(est_ivb_DU, contaminant = AsPb) |> # convert to rba
            fy_error(contaminant = AsPb) |>     # convert to ivba with model error
            fx(contaminant = AsPb)               # convert to rba again
        }else{
          fx(est_ivb_DU, contaminant = AsPb)     # just convert to rba (model error previously applied or not at all)
        }),
      by = "sim.num"
    ) |>
    dplyr::mutate(ba_DU = est_tot_DU*est_rba_DU/100) |>
    dplyr::mutate(errYN = if(frcAct>=0){
      as.numeric(ba_DU<actLvl.adj)
    }else{
      as.numeric(ba_DU>actLvl.adj)
    })

  err_pb <- DU.sim |>
    dplyr::summarize(
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
