#' Take samples
#'
#' @param n.samp Number of samples
#' @param n.incr Number of increments
#' @param n.sim Number of simulations
#' @param distfun Distribution function
#' @param ... Additional arguments to the distribution function
#'
#' @return Dataframe with simulation values and attributes.
#' @export
#'
#' @examples take_samples(5, 2, 20, "simDist_rba", 50, 0.3, "lognorm")
take_samples <- function(
    n.samp,
    n.incr,
    n.sim,
    distfun,
    ...
){
  n.cores <- n.sim*n.samp*n.incr
  simvalues <- do.call(distfun, args = append(n.cores, list(...)))

  # long output
  out.long <- data.frame(
    sim.num = rep(seq(1:n.sim), each = n.samp*n.incr),
    sample.num = rep(rep(seq(1:n.samp), each = n.incr), n.sim),
    incr.num = rep(rep(seq(1:n.incr), times = n.samp)),
    sim.value = simvalues
  )

  return(out.long)
}
