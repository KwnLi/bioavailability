#' Inverse model to calculate observed IVBA from true RBA with model error
#'
#' @param y "true" RBA
#' @param contaminant string, "As" or "Pb"
#' @param sepred (Optional) a custom `sepred` value. Default is to use the
#'   sepred value in `contam_params` object for `contaminant` "As" or "Pb"
#'
#' @return Estimated IVBA value based on RBA defined by `y` with error
#' @export
#'
#' @examples
#' fy_error(65, "As")
#'
fy_error <- function(y, contaminant, sepred = NULL){
  m = contam_params[[contaminant]]["m"]
  b = contam_params[[contaminant]]["b"]
  sepred = ifelse(is.null(sepred), contam_params[[contaminant]]["sepred"], sepred)

  x = (y - rnorm(1,0,sepred) - b)/m

  return(max(0,x))
}
fy_error <- Vectorize(fy_error)
