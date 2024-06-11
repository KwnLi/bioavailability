#' Function to generate "true" total conc.
#'
#' @param n.totmeas Number of total measurements
#' @param tru_mu_tot "True" mean total concentration
#' @param coeV_tot coefficient of variation of total concentration
#' @param dist_tot distribution of total concentration measurements; options are
#'   "normal" or "lognorm"
#'
#' @return A vector of simulated total concentration measurements
#' @export
#'
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
