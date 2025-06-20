#' Step 1 function
#'
#' Determine sequence of sample sizes to simulate
#'
#' @param AsPb define contaminant
#' @param tot.n number of total conc. samples, passed to simDU
#' @param ivba.n number of ivba samples, passed to simDU
#' @param sampmax maximum number of samples to simulate, default 10*tot.n
#' @param incr.vec vector of increments to test
#' @param ... simDU parameters
#'
#' @return simulation results
#' @export
#'
#' @examples
#' test.step1 <- step1(AsPb = "Pb", tot.n = 5, ivba.n = 3, sampmax = 50,
#' frcAct = -0.25, coeV.tot = 0.5, coeV.rba = 0.3, error_tot = TRUE,
#' error_ivb = TRUE, error_ivb_cv = 0.05, ivba_model = TRUE, incr.vec = c(1,5))
#'
step1 <- function(AsPb = NULL, tot.n = NULL, ivba.n = NULL, sampmax = NULL,
    incr.vec = NULL, ...
){
  sampout <- length(seq(max(c(tot.n, ivba.n)), sampmax, by=1))
  tot.range <- seq(tot.n, by=1, length.out = sampout)
  ivba.range <- seq(ivba.n, by=1, length.out = sampout)

  step1.out <- list()

  for(h in 1:length(incr.vec)){
    step1.h <- list()
    for(i in 1:sampout){
      # progress message
      print(paste("Simulating", tot.range[i], "total concentration samples and",
                  ivba.range[i], "IVBA samples, with", incr.vec[h], "increments", sep = " "))

      step1.h[[i]] <- simDU(
        AsPb = AsPb,
        tot.n = tot.range[i],
        ivba.n = ivba.range[i],
        outputLvl = 1,
        tot.incr = incr.vec[h],
        ivba.incr = incr.vec[h],
        ...
      )$err_pb
    }
    step1.out[[h]] <- dplyr::bind_rows(step1.h) |> dplyr::mutate(n_incr = incr.vec[h])
  }


  return(list(step1 = dplyr::bind_rows(step1.out), AsPb = AsPb))
}
