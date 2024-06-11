#' Test the coev back calculation
#'
#' Function returns incremented x by taking means in groups of 'incr'. Used to
#' demonstrate that as effective number of "cores" increases (i.e., base unit of
#' sampling whether composited or not), e estimated coeV gets closer to the
#' original inputted coeV (sd=20, mn = 300)
#'
#' @param x vector of increment samples, values represent individual increments
#' @param incr numeric value of increment size, increment grouping size of
#'   samples
#'
#' @return vector of "composited" samples, number of samples = x/incr
#' @export
testcv <- function(x, incr){
  x.mat <- matrix(x, ncol = incr)
  return(rowMeans(x.mat))
}
