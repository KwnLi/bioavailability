#' Tool disclaimer
#'
#' Returns disclaimer text
#'
#' @return an html tag with tool disclaimer text
#' @export
#'
tool_notes <- function(){
  HTML(
  "<b>Notes:</b>
  <br/><b><i>EPC</i></b> = Exposure Point Concentration*
  <br/><b><i>DU</i></b> = Decision Unit
  <br/><b><i>AL</i></b> = Action Level
  <br/>* Here, the EPC is defined as the bioavailability-adjusted soil metal
  concentration (i.e., the total metal concentration (mg/kg) x % RBA),
  which is compared to an action level that has not been adjusted for
  bioavailability. In cases where a % RBA value other than 100% was input
  in the Assumed RBA of the action level field when defining the action level,
  the tool converts the RBA-adjusted action level to an action level that has
  not been adjusted for RBA by multiplying the user input action level by the
  user input assumed RBA of the action level (%)."
  )
}
