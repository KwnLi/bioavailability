#' Step 2 function
#'
#' Vary the contaminant level
#'
#' @param AsPb define contaminant
#' @param tot.n number of total conc. samples, passed to simDU
#' @param ivba.n number of ivba samples, passed to simDU
#' @param minFrcAct minimum fraction of action level to simulate
#' @param maxFrcAct maximum fraction of action level to simulate
#' @param numbins nr. divisions over range of contaminant levels
#' @param ... simDU parameters
#'
#' @returns A list with error probabilities at the simulated quantiles and a
#'   character string indicating As or Pb
#' @export
#'
#' @examples
#' test.step2.2 <- step2(AsPb = "Pb", tot.n = 5, ot.incr = 10, ivba.n = 3,
#' ivba.incr = 10, minFrcAct = -0.01, maxFrcAct = -0.99, coeV.tot = 0.5,
#' coeV.rba = 0.05, error_tot = TRUE, error_ivb = TRUE, error_ivb_cv = 0.05,
#' ivba_model = TRUE)
#'
step2 <- function(AsPb = NULL, tot.n = NULL, ivba.n = NULL, minFrcAct = NULL,
                  maxFrcAct = NULL, numbins = 20, ...
){
  if(minFrcAct*maxFrcAct < 0){stop("Min and max fraction of action level range cannot span zero")}

  # make sure minFrcAct is less than maxFrcAct
  if(minFrcAct > maxFrcAct){
    tempval <- maxFrcAct
    maxFrcAct <- minFrcAct
    minFrcAct <- tempval
  }

  frcActRange <- round(seq(from = ifelse(minFrcAct>-1,minFrcAct,-0.99),
                           to = ifelse(maxFrcAct>-1,maxFrcAct,-0.01),
                           length.out = numbins), 2)

  err_pb <- list()
  simQt <- list()
  for(i in 1:length(frcActRange)){
    # progress message
    print(paste("Simulating", 100*abs(frcActRange[i]), "%",
                ifelse(frcActRange[i]<0,"below","above"),
                "the action level", sep = " "))
    sim.i <- simDU(
      AsPb = AsPb,
      tot.n = tot.n,
      ivba.n = ivba.n,
      frcAct = frcActRange[i],
      outputLvl = 2,
      ...
    )

    err_pb[[i]] <- sim.i$err_pb
    simQt[[i]] <- quantile(sim.i$DU_sim$ba_DU, probs = c(0.05, 0.25, 0.50, 0.75, 0.95))
  }

  simQt <- dplyr::bind_rows(simQt) |> dplyr::rename_with(.fn = ~paste0("ba_sim_", .))

  step2.out <- dplyr::bind_rows(err_pb) |> dplyr::bind_cols(simQt)

  return(list(step2 = step2.out, AsPb = AsPb))
}
