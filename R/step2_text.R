#' Step 2 text functions
#'
#' @param step2_t1 Type 1 error results
#' @param step2_t2 Type 2 error results
#' @param error_threshold Desired error threshold
#'
#' @return A text string.
#' @export
#'
#' @rdname step2_text
step2_text_t1 <- function(step2_t1, error_threshold){
  t1_thresh <- error_threshold/100

  type1_df <- step2_t1$step2 |>
    dplyr::mutate(from.t1 = err_pb - t1_thresh) |> # find difference from t1 threshold
    dplyr::mutate(dif.neg = from.t1 <= 0) |> # sign of difference
    dplyr::mutate(lag.neg = dplyr::lag(dif.neg), lead.neg = dplyr::lead(dif.neg))

  # sort by absolute difference from threshold and find 2 closest
  nearest.type1 <- type1_df |> dplyr::filter(from.t1 < 0) |> dplyr::filter(from.t1 == max(from.t1)) |>
    dplyr::bind_rows(type1_df |> dplyr::filter(from.t1 > 0) |> dplyr::filter(from.t1 == min(from.t1)))

  # find where error crosses threshold (false compliance/type1)
  if(nrow(nearest.type1)!=2){ # test if both same logical (==1)
    message.type1 <- " the sampling protocol evaluated does not achieve target false compliance (Type 1) decision error probabilities across the range in the DU's true EPC assessed."
  }else{
    frcAct.t1 <- interp(x=nearest.type1$frcAct, y=nearest.type1$err_pb, middle.point.y=t1_thresh)
    tru_ba.t1 <- interp(x=nearest.type1$tru_ba, y=nearest.type1$err_pb, middle.point.y=t1_thresh)

    message.type1 <- paste0(", so long as the DU's true bioavailability-adjusted EPC is greater than ",
                            round(tru_ba.t1, 0), " mg bioavailable ", step2_t1$AsPb, " (i.e., greater than ",
                            round(100*frcAct.t1, 1), "% above the ",
                            nearest.type1$actLvl.adj[1], " ppm AL), the sampling protocol evaluated will achieve satisfactory decision error probability goals for false compliance.")
  }

  return(paste0("<b>False compliance error:</b> the simulation estimates that", message.type1))
}

#' @rdname step2_text
step2_text_t2 <- function(step2_t2, error_threshold){
  t2_thresh <- error_threshold/100
  type2_df <- step2_t2$step2 |>
    dplyr::mutate(from.t2 = err_pb - t2_thresh) |> # find difference from t2 threshold
    dplyr::mutate(dif.neg = from.t2 <= 0) |> # sign of difference
    dplyr::mutate(lag.neg = dplyr::lag(dif.neg), lead.neg = dplyr::lead(dif.neg))

  # sort by absolute difference from threshold and find 2 closest
  nearest.type2 <- type2_df |> dplyr::filter(from.t2 < 0) |> dplyr::filter(from.t2 == max(from.t2)) |>
    dplyr::bind_rows(type2_df |> dplyr::filter(from.t2 > 0) |> dplyr::filter(from.t2 == min(from.t2)))

  # find where error crosses threshold (false exceedance/type 2)
  if(nrow(nearest.type2)!=2){ # test if both same logical (==1)
    message.type2 <- " the sampling protocol evaluated does not achieve target false exceedance (Type 2) decision error probabilities across the range in the DU's true EPC assessed."
  }else{
    frcAct.t2 <- interp(x=nearest.type2$frcAct, y=nearest.type2$err_pb, middle.point.y=t2_thresh)
    tru_ba.t2 <- interp(x=nearest.type2$tru_ba, y=nearest.type2$err_pb, middle.point.y=t2_thresh)

    message.type2 <- paste0(", so long as the DU's true bioavailability-adjusted EPC is less than ",
                            round(tru_ba.t2, 0), " mg bioavailable ", step2_t2$AsPb, " (i.e., less than ",
                            round(100*abs(frcAct.t2), 1), "% below the ",
                            nearest.type2$actLvl.adj[1], " ppm AL), the sampling protocol evaluated will achieve satisfactory decision error probability goals for false exceedance")
  }

  return(paste0("<b>False exceedance error:</b> the simulation estimates that", message.type2))
}
