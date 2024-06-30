#' Find interpolated value
#'
#' Function to find interpolated value given two points (err_pb is the y)
#'
#' @param x x
#' @param y y
#' @param middle.point.y
#'
#' @returns Interpolated value between the first and second values of `x`
#' @export
#'
#' @examples
#' a=c(1,4)
#' b=c(4,6)
#' interp(a,b,5)
interp <- function(x, y, middle.point.y){
  slope = (y[2]-y[1])/(x[2]-x[1])
  return(x[1] + (middle.point.y - y[1])/slope)
}
