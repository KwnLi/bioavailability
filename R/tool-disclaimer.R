#' Tool disclaimer
#'
#' Returns disclaimer text
#'
#' @return an html tag with tool disclaimer text
#' @export
#'
tool_disclaimer <- function(){
  HTML(
    "<b>Disclaimers:</b>
      <br/><ul><li>Use of this tool requires assumptions that may not be accurate
      <li>The tool estimates false compliance and false exceedance decision error
      probability based primarily on uncertainty in a) representativeness of samples
      collected with respect to average conditions across the geographic scale of
      the decision unit and b) analytical measurements of total metal concentration
      and % IVBA. Other factors not considered by the simulation model may impact
      decision error probability, including but not limited to uncertainty in the
      EPA validated methods used to convert measurements of soil PB or As IVBA to
      RBA.</li><ul>"
  )
}
