#' Custom log-normal random number generator
#'
#' A wrapper for the rlnorm function that converts the normal mean and sd inputs
#' so that the output lognormal samples have the specified mean and sd at the
#' lognormal scale
#'
#' @param n number of returned observations
#' @param m mean in lognormal scale
#' @param s sd in lognormal scale
#'
#' @return Values drawn from a lognormal distribution with mean `m` and standard
#'   deviation `s` in the lognormal scale.
#' @export
#'
#' @examples
#' custom_rlnorm(10, 5, 3)
#'
custom_rlnorm <- function(n, m, s){
  location <- log(m^2 / sqrt(s^2 + m^2))
  shape <- sqrt(log(1 + (s^2 / m^2)))
  return(rlnorm(n=n, location, shape))
}
