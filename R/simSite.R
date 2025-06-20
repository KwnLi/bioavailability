#' Simulate site function
#'
#' @param mn_rba_site mean "true" RBA for the whole site
#' @param coeV_rba_site coefficient of varation of RBA for the whole site
#' @param simDist_rba_site distribution of DU RBA value across the site
#' @param coeV_rba_DU coefficient of variation of RBA within a DU
#' @param simDist_rba_DU distribution type of DU sample values
#' @param DU.n number of DUs
#' @param ivba.n number of IVBA samples
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
    coeV_rba_DU,
    simDist_rba_DU,
    ivba.n,
    ivba.incr,
    error_ivb = TRUE,
    ivba_model = FALSE,
    post_mean = FALSE,
    error_ivb_cv,
    useMeanIVBA = TRUE,
    iter = 5000
  ){

  AsPb <- "Pb"  # only available for Pb

  rba.sims <- list()
  for(i in seq_len(iter)){

    # use take_samples function to draw DU means
    DU.mn.i <- take_samples(n.samp = DU.n, n.incr = 1, n.sim = 1,
                            "simDist_rba", tru_mu_rba=mn_rba_site,
                            coeV_rba=coeV_rba_site, dist_rba=simDist_rba_site) |>
      dplyr::pull(sim.value)

    # apply take_samples to the DU means to create sample values
    rba.sims[[i]] <- lapply(DU.mn.i,
      FUN = function(x){
        take_samples(n.samp = ivba.n, n.incr = ivba.incr, n.sim = 1,
                     "simDist_rba", tru_mu_rba=x,
                     coeV_rba=coeV_rba_DU, dist_rba=simDist_rba_DU)
          # dplyr::select(-n.sim)
      }) |>
      dplyr::bind_rows(.id = "DU") |> # combine the DU simulations for the iteration into a dataframe
      dplyr::bind_cols(dplyr::tibble(DU_mn = rep(DU.mn.i, each = ivba.n*ivba.incr)))
  }

  rba.sims <- rba.sims |> dplyr::bind_rows(.id = "iter")

  rba.sim.meas <- rba.sims |>
    dplyr::group_by(iter, sample.num, DU, DU_mn) |>
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
  DU.sim <- rba.sim.meas |> dplyr::group_by(iter, DU, DU_mn) |>
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
    })

  # Calculate DU error and sitewide error
  DU.error <- DU.sim |>
    dplyr::group_by(iter) |>
    dplyr::mutate(est_rba_site = mean(est_rba_DU)) |>
    dplyr::ungroup() |>
    dplyr::mutate(DU_error = 100*(est_rba_site - DU_mn)/DU_mn,
                  DU_abserror = abs(DU_error)) |>
    dplyr::mutate(iter = as.numeric(iter), DU = as.numeric(DU)) |>
    dplyr::arrange(iter, DU) |>
    as.data.frame()

  site.error <- DU.error |>
    dplyr::mutate(
      site_error = 100*(est_rba_site - mn_rba_site)/mn_rba_site,
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
      DU_abserror_mean = mean(DU_abserror_mean),
      DU_abserror_lowerci = mean(DU_abserror_lowerci),
      DU_abserror_upperci = mean(DU_abserror_upperci),
      DU_abserror_max = mean(DU_abserror_max)
    ) |>
    as.data.frame()

  return(list(DU_error = DU.error, site_error = site.error, sim_error = sim.error))
}


# simSite(0.4, 0.1, "lognorm", 0.1, "lognorm", 15, 7, 3, error_ivb_cv = 0.05)

