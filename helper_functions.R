#####Custom lognormal#####
# just a wrapper form the rlnorm that converts the normal mean and sd
# inputs so that the output lognormal samples have the specified mean and sd
# at the lognormal scale

custom_rlnorm <- function(n, m, s){
  location <- log(m^2 / sqrt(s^2 + m^2))
  shape <- sqrt(log(1 + (s^2 / m^2)))
  return(rlnorm(n=n, location, shape))
}

# upper 95% conf. int. of the mean function:
upper95 <- function(x,lvl){
  xbar = mean(x)
  up95 = xbar + qnorm(lvl)*(sd(x)/sqrt(length(x)))
  return(up95)
}

