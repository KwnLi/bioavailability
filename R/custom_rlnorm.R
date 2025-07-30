#' Custom log-normal random number generator
#'
#' A wrapper for the rlnorm function that converts the normal mean and sd inputs
#' so that the output lognormal samples have the specified mean and sd at the
#' lognormal scale
#'
#' @param n number of returned observations
#' @param m mean in lognormal scale
#' @param s sd in lognormal scale
#' @param truncate T/F truncate the upper end of distribution?
#' @param max maximum allowable value, if truncating. Default = 1
#'
#' @return Values drawn from a lognormal distribution with mean `m` and standard
#'   deviation `s` in the lognormal scale.
#' @export
#'
#' @examples
#' custom_rlnorm(10, 5, 3)
#'
custom_rlnorm <- function(n, m, s, truncate = FALSE, max=1){
  location <- log(m^2 / sqrt(s^2 + m^2))
  shape <- sqrt(log(1 + (s^2 / m^2)))

  outdist <- rlnorm(n=n, location, shape)

  # TODO(maybe): find a method to actually have correct mean and sd
  # https://stats.stackexchange.com/questions/408171/compute-truncated-normal-distribution-with-specific-mean-and-variance
  # https://doi.org/10.1016/j.spl.2015.05.006

  if(truncate){
    while(sum(outdist>max)>0){
      overmax <- which(outdist>max)
      outdist[overmax] <- rlnorm(n=length(overmax), location, shape)
    }
  }
  return(outdist)
}
