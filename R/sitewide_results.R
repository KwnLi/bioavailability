#' Plotting functions for sitewide app
#'
#' @param site_error site error table
#' @param tru.mean true mean value set by user
#'
#' @returns
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

hist.DU_abserror_siteRBA <- function(site_error){
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


# test <- read.csv("/Users/kevinl/Downloads/sitewide_simdata 5/out3_site_error.csv")

