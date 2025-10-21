#' Simulate site function
#'
#' @param mn_rba_site mean "true" RBA for the whole site
#' @param coeV_rba_site coefficient of varation of RBA for the whole site
#' @param simDist_rba_site distribution of DU RBA value across the site
#' @param DU.n number of DUs
#' @param sample.n number of samples measuring IVBA
#' @param ivba.incr number of increments per IVBA sample
#' @param error_ivb (TRUE/FALSE) apply IVBA measurement error? Default is FALSE.
#' @param ivba_model (TRUE/FALSE) apply IVBA model error? Default is FALSE.
#' @param post_mean (TRUE/FALSE) calculate IVBA model error after
#'   summarizing across samples. Default is FALSE.
#' @param error_ivb_cv IVBA measurement error coefficient of variance
#' @param useMeanIVBA (TRUE/FALSE) use the mean IVBA in calculating error results?
#' @param iter Number of simulation iterations
#'
#' @returns simulation results
#' @export
#'
simSite <- function(
    DU.n,
    sample.n,
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
  rba.sims <- take_samples(n.samp = sample.n, n.incr = ivba.incr, n.sim = iter,
                           "simDist_rba", tru_mu_rba=mn_rba_site,
                           coeV_rba=coeV_rba_site, dist_rba=simDist_rba_site)

  DU.sims <- do.call("simDist_rba",
                     args = list(n.rbameas=DU.n*iter,
                                 tru_mu_rba=mn_rba_site,
                                 coeV_rba=coeV_rba_site,
                                 dist_rba=simDist_rba_site))

  rba.sim.meas <- rba.sims |>
    dplyr::group_by(sim.num, sample.num) |>
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
  sample.sim <- rba.sim.meas |> dplyr::group_by(sim.num, sample.num) |>
    dplyr::mutate(est_rba_sample = if(ivba_model & post_mean){  # TRUE/FALSE if modeling ivba AFTER taking mean
      fx(meas.ivb, contaminant = AsPb) |>  # convert to rba
        fy_error(contaminant = AsPb) |>      # convert to ivba with model error
        fx(contaminant = AsPb)               # convert to rba again
    }else{
      fx(meas.ivb, contaminant = AsPb)     # just convert to rba (if model error previously applied or not at all)
    })

  # Calculate sample values for sites
  sample.values <- sample.sim |>
    dplyr::group_by(sim.num) |>
    dplyr::mutate(est_rba_site = mean(est_rba_sample)) |>
    dplyr::ungroup() |>
    dplyr::mutate(sim.num = as.numeric(sim.num), sample.num = as.numeric(sample.num)) |>
    dplyr::arrange(sim.num, sample.num) |>
    as.data.frame()

  # Set up DU values for sites
  DU.values <- data.frame(sim.num = rep(1:iter, each = DU.n), tru_DU_rba = DU.sims) |>
    dplyr::left_join(sample.values |> dplyr::select(sim.num, est_rba_site) |> dplyr::distinct(),
                     by = "sim.num") |>
    dplyr::mutate(
      DU_error_siteRBA = est_rba_site - tru_DU_rba,
      DU_abserror_siteRBA = abs(est_rba_site - tru_DU_rba)
    )

  site.DU.error <- DU.values |>
    dplyr::group_by(sim.num, est_rba_site) |>
    dplyr::summarize(
      DU_error_siteRBA_mean = mean(DU_error_siteRBA, na.rm = TRUE),
      DU_abserror_siteRBA_mean = mean(DU_abserror_siteRBA, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      siteRBA_error = est_rba_site-mn_rba_site,
      siteRBA_abserror = abs(est_rba_site-mn_rba_site),
      sim.num = as.numeric(sim.num)
      ) |>
    as.data.frame()

  sim.error <- site.DU.error |>
    dplyr::summarize(
      siteRBA_abserror_mean = mean(siteRBA_abserror, na.rm = TRUE),
      siteRBA_abserror_lowerci = quantile(siteRBA_abserror,.025, na.rm = TRUE),
      siteRBA_abserror_upperci = quantile(siteRBA_abserror,.975, na.rm = TRUE),
      siteRBA_abserror_max = max(siteRBA_abserror, na.rm = TRUE),

      DU_abserror_siteRBA_mean_mean = mean(DU_abserror_siteRBA_mean),
      DU_abserror_siteRBA_lowerci = quantile(DU_abserror_siteRBA_mean,.025, na.rm = TRUE),
      DU_abserror_siteRBA_upperci = quantile(DU_abserror_siteRBA_mean,.975, na.rm = TRUE),
      DU_abserror_siteRBA_meanDU_max = max(DU_abserror_siteRBA_mean, na.rm = TRUE),
      DU_abserror_siteRBA_allDU_max = max(DU.values$DU_abserror_siteRBA, na.rm = TRUE)
    ) |>
    as.data.frame()

  return(list(out1_samples = sample.values, out2_DU_values = DU.values,
              out3_site_error = site.DU.error, out4_sim_error = sim.error))
}

