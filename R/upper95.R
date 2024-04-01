#' Upper 95% conf. int. of the mean function
#'
#' This is a convenience function for calculating the 95% quantile of a vector
#' of observations `x`, intended for calculating the upper 95% confidence value
#'
#' @param x observation values
#' @param lvl desired quantile of `x` to calculate
#'
#' @return
#' @export
#'
#' @examples
#' upper95(x=c(29,39,48,34,58), lvl=0.95)
upper95 <- function(x,lvl){
  xbar = mean(x)
  up95 = xbar + qnorm(lvl)*(sd(x)/sqrt(length(x)))
  return(up95)
}
