#' Function to generate "true" rba
#'
#' @param n.rbameas Number of RBA measurements
#' @param tru_mu_rba Mean "true" RBA value
#' @param coeV_rba Coefficient of variation of RBA distribution
#' @param dist_rba Distribution of RBA measurements; can be "normal", "lognorm",
#'   or "uniform"
#'
#' @return A vector of simulated RBA measurements
#' @export
#'
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
