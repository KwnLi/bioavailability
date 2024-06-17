decision_obj_input <- function(id){
  tagList(
    shinyjs::useShinyjs(),
    sliderInput(NS(id, "type1error"), label = "False compliance error threshold (%)",
                value = 5, step = 0.5,
                min = 0, max = 100,
                round = TRUE),
    sliderInput(NS(id, "type2error"), label = "False exceedance error threshold (%)",
                value = 20, step = 0.5,
                min = 0, max = 100,
                round = TRUE),
    verbatimTextOutput(NS(id, "info"))
  )
}

decision_obj_server <- function(id, info=FALSE){
  moduleServer(id, function(input, output, session){
    shinyjs::toggleElement("info", condition = info)

    out <- reactiveValues()

    observe({
      out$type1error <- input$type1error
      out$type2error <- input$type2error
    })

    out
  })
}
