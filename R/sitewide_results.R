#' Plotting functions for sitewide app
#'
#' @param site_error site error table
#' @param tru.mean true mean value set by user
#'
#' @returns a plot
#' @export
#'
hist.est_rba_site <- function(site_error, tru.mean){
  obs.mean <- mean(site_error$est_rba_site)
  obs.95 <- quantile(site_error$est_rba_site, c(0.025, 0.975))
  ggplot(site_error, aes(est_rba_site)) + geom_histogram() +
    geom_vline(xintercept = tru.mean, color = "red") +
    # geom_vline(xintercept = obs.mean, color = "blue") +
    geom_vline(xintercept = obs.95[1], color = "red", lty = 2) +
    geom_vline(xintercept = obs.95[2], color = "red", lty = 2) +
    ggtitle("Distribution of estimated site RBA values") +
    annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
             label=paste0("mean = ",round(tru.mean,2), " (95% CI: ", round(obs.95[1],2),", ", round(obs.95[2],2), ")")
             )
}

#' @export
hist.DU_abserror_siteRBA_mean <- function(site_error){
  obs.mean <- mean(site_error$DU_abserror_siteRBA_mean)
  obs.95 <- quantile(site_error$DU_abserror_siteRBA_mean, c(0.025, 0.975))
  ggplot(site_error, aes(DU_abserror_siteRBA_mean)) + geom_histogram() +
    geom_vline(xintercept = obs.mean, color = "red") +
    geom_vline(xintercept = obs.95[1], color = "red", lty = 2) +
    geom_vline(xintercept = obs.95[2], color = "red", lty = 2) +
    ggtitle("Distribution of DU absolute error means") +
    annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
             label=paste0("mean = ",round(obs.mean,2), " (95% CI: ", round(obs.95[1],2),", ", round(obs.95[2],2), ")")
    )

}

#' @export
hist.DU_abserror_siteRBA <- function(DU_values){
  # error.freq <- table(ggplot2::cut_width(DU_values$DU_abserror_siteRBA, 1, boundary = 0)) |>
  #   as.data.frame()
  obs.mean <- mean(DU_values$DU_abserror_siteRBA)
  obs.5 <- quantile(DU_values$DU_abserror_siteRBA, c(0.95))
  ggplot(DU_values, aes(DU_abserror_siteRBA)) +
    geom_histogram(aes(y = after_stat(count / sum(count))),binwidth=1) +
    scale_y_continuous(labels = scales::percent) +
    # geom_vline(xintercept = obs.mean, color = "red") +
    geom_vline(xintercept = obs.5[1], color = "red", lty = 2) +
    # geom_vline(xintercept = obs.95[2], color = "red", lty = 2) +
    ggtitle("Frequency of absolute error in RBA (DU)") +
    annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
             label=paste0("mean = ",round(obs.mean,2), ", 95% threshold = ", round(obs.5[1],2))
    )
}

#' @export
result_text <- function(result, params){
  est_rba_site_obs.95 <- round(quantile(result()$out3_site_error$est_rba_site, c(0.025, 0.975)),1)
  DU_abserror_siteRBA_mean <- round(mean(result()$out3_site_error$DU_abserror_siteRBA_mean),1)
  DU_abserror_siteRBA.95 <- round(quantile(result()$out2_DU_values$DU_abserror_siteRBA, c(0.95)),1)
  paste(
    "<p>When collecting ",
    params$sample.n,
    ifelse(params$ivba.incr>1," composite"," discrete"),
    " samples across a site with ",
    params$DU.n,
    " DUs, where the assumed true sitewide RBA = ",
    params$mn_rba_site,
    "%, and the assumed true variability (CoV) in RBA across the site = ",
    params$coeV_rba_site,
    ", the tool estimates that 95% of sampling efforts will result in an estimate of sitewide RBA between ",
    est_rba_site_obs.95[1], "-", est_rba_site_obs.95[2],
    "% (first graph). When applying a sitewide RBA value to individual DUs, the tool estimates that <b>1)</b> the \"average\" absolute error in RBA(DU) will be ~ ",
    DU_abserror_siteRBA_mean,
    "% (second graph), and <b>2)</b> 5% of the time, the abs. error in any single RBA(DU) will be > ",
    DU_abserror_siteRBA.95,
    "% (third graph) . </p>"
  )
}
# test <- read.csv("/Users/kevinl/Downloads/sitewide_simdata 5/out3_site_error.csv")
# test2 <- read.csv("/Users/kevinl/Downloads/sitewide_simdata 5/out2_DU_values.csv")

