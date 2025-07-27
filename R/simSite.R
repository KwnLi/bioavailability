#' Simulate site function
#'
#' @param mn_rba_site mean "true" RBA for the whole site
#' @param coeV_rba_site coefficient of varation of RBA for the whole site
#' @param simDist_rba_site distribution of DU RBA value across the site
#' @param DU.n number of DUs
#' @param ivba.incr number of increments per IVBA sample
#' @param error_ivb (TRUE/FALSE) apply IVBA measurement error? Default is FALSE.
#' @param ivba_model (TRUE/FALSE) apply IVBA model error? Default is FALSE.
#' @param post_mean (TRUE/FALSE) calculate IVBA model error after
#'   summarizing across samples. Default is FALSE.
#' @param error_ivb_cv IVBA model error coefficient of variance
#' @param useMeanIVBA (TRUE/FALSE) use the mean IVBA in calculating error results?
#' @param iter Number of simulation iterations
#'
#' @returns simulation results
#' @export
#'
simSite <- function(
    DU.n,
    mn_rba_site,
    coeV_rba_site,
    simDist_rba_site,
    ivba.incr,
    error_ivb = TRUE,
    ivba_model = FALSE,
    post_mean = FALSE,
    error_ivb_cv,
    iter = 5000
  ){

  AsPb <- "Pb"  # only available for Pb

  # use take_samples function to draw DU means
  # sample.num = DU; incr.num = increment
  rba.sims <- take_samples(n.samp = DU.n, n.incr = ivba.incr, n.sim = iter,
                           "simDist_rba", tru_mu_rba=mn_rba_site,
                           coeV_rba=coeV_rba_site, dist_rba=simDist_rba_site) |>
    dplyr::rename(iter = "sim.num", DU = "sample.num")

  rba.sim.meas <- rba.sims |>
    dplyr::group_by(iter, DU) |>
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
  DU.sim <- rba.sim.meas |> dplyr::group_by(iter, DU) |>
    dplyr::mutate(est_rba_DU = if(ivba_model & post_mean){  # TRUE/FALSE if modeling ivba AFTER taking mean
      fx(meas.ivb, contaminant = AsPb) |>  # convert to rba
        fy_error(contaminant = AsPb) |>      # convert to ivba with model error
        fx(contaminant = AsPb)               # convert to rba again
    }else{
      fx(meas.ivb, contaminant = AsPb)     # just convert to rba (model error previously applied or not at all)
    })

  # Calculate DU error and sitewide error
  DU.error <- DU.sim |>
    dplyr::group_by(iter) |>
    dplyr::mutate(est_rba_site = mean(est_rba_DU)) |>
    dplyr::ungroup() |>
    dplyr::mutate(DU_error = est_rba_site - mn_rba_site,
                  DU_abserror = abs(DU_error)) |>
    dplyr::mutate(iter = as.numeric(iter), DU = as.numeric(DU)) |>
    dplyr::arrange(iter, DU) |>
    as.data.frame()

  site.error <- DU.error |>
    dplyr::mutate(
      site_error = est_rba_site - mn_rba_site,
      site_abserror = abs(site_error),
    ) |>
    dplyr::group_by(iter, site_error, site_abserror) |>
    dplyr::summarize(
      DU_error_mean = mean(DU_error, na.rm = TRUE),
      DU_error_lowerci = quantile(DU_error,.025, na.rm = TRUE),
      DU_error_upperci = quantile(DU_error,.975, na.rm = TRUE),
      DU_error_max = max(DU_error, na.rm = TRUE),
      DU_abserror_mean = mean(DU_abserror, na.rm = TRUE),
      DU_abserror_lowerci = quantile(DU_abserror,.025, na.rm = TRUE),
      DU_abserror_upperci = quantile(DU_abserror,.975, na.rm = TRUE),
      DU_abserror_max = max(DU_abserror, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(iter = as.numeric(iter)) |>
    as.data.frame()

  sim.error <- site.error |>
    dplyr::summarize(
      site_abserror_mean = mean(site_abserror),
      site_abserror_lowerci = quantile(site_abserror,.025, na.rm = TRUE),
      site_abserror_upperci = quantile(site_abserror,.975, na.rm = TRUE),
      site_error_mean = mean(site_error),
      site_error_lowerci = quantile(site_error_mean,.025, na.rm = TRUE),
      site_error_upperci = quantile(site_error_mean,.975, na.rm = TRUE),
    ) |>
    as.data.frame()

  return(list(DU_error = DU.error, site_error = site.error, sim_error = sim.error))
}

