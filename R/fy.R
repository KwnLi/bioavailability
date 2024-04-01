#' Inverse model to calculate observed IVBA from true RBA without model error
#'
#' Calculates IVBA based on provided RBA without model error.
#'
#' @param y "true" RBA
#' @param contaminant string, "As" or "Pb"
#'
#' @return Estimated IVBA value based on RBA defined by `y`
#' @export
#'
#' @examples
#' fy_error(65, "As")
#'
fy <- function(y, contaminant){
  m = contam_params[[contaminant]]["m"]
  b = contam_params[[contaminant]]["b"]

  x = (y - b)/m

  return(max(0,x))
}
fy <- Vectorize(fy)
