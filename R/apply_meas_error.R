#' Measurement error
#'
#' Apply measurement error
#'
#' @param tru.val True value vector
#' @param max.val Maximum value (upper bound of rtruncnorm function)
#' @param coefVar coefficient of variation of error spread around mean
#'
#' @return A value drawn from a distribution specified by the parameters
#' @export
#'
apply_meas_error <- function(tru.val = NULL, max.val = Inf, coefVar = 0.05
){
  meas.tot <- truncnorm::rtruncnorm(n=length(tru.val), a=0, b=max.val,
                         mean = tru.val,
                         sd = mean(tru.val)*coefVar)
  return(meas.tot)
}
