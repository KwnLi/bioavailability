#' Direct model, from mean measured IVBA to estimated RBA
#'
#' @param x measured IVBA
#' @param contaminant string, "As" or "Pb"
#'
#' @return Estimated RBA value based on measured IVBA `x`
#' @export
#'
#' @examples
#' fx(75, "Pb")
#'
fx <- function(x, contaminant){
  m = contam_params[[contaminant]]["m"]
  b = contam_params[[contaminant]]["b"]

  y = m*x+b

  return(y)
}
fx <- Vectorize(fx)
