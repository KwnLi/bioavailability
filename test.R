

ui <- fluidPage(
  contaminant_input("a"),
  verbatimTextOutput("info")
)

serv <- function(input, output, session){
  a <- contaminant_server("a")
  output$info <- renderPrint(reactiveValuesToList(a))
}

shinyApp(ui, serv)

ui <- fluidPage(
  samples_input("a")
)

serv <- function(input, output, session){
  a <- samples_server("a", inputn=TRUE, askComposite = TRUE)
}


shinyApp(ui, serv)

ui <- fluidPage(
  step1_input("a")
)

serv <- function(input, output, session){
  a <- step1_server("a")
}


shinyApp(ui, serv)


ui <- fluidPage(
  decision_obj_input("a")
)

serv <- function(input, output, session){
  a <- decision_obj_server("a", info=TRUE)
}


shinyApp(ui, serv)

ui <- fluidPage(
  du_assum_input("a")
)

serv <- function(input, output, session){
  a <- du_assum_server("a")
}


shinyApp(ui, serv)

ui <- fluidPage(
  sim_params_input("a")
)

serv <- function(input, output, session){
  a <- sim_params_server("a", info=TRUE)
}


shinyApp(ui, serv)

ui <- fluidPage(
  step1a_interface("a"),
  step1a_output("b")
)

serv <- function(input, output, session){
  a <- step1a_interface_server("a", contam= reactiveValues(a=12), info=TRUE)
  step1a_output_server("b", a)
}


shinyApp(ui, serv)
