step2_input <- function(id){
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(width = 12, h4("Simulation range above/below action level:"))
    ),
    fluidRow(
      column(width = 4,
             numericInput(NS(id,"minFrcAct"), label = div(style = "font-weight: normal; font-style: italic", "Minimum (%):"), value = 0, min = 0)
      ),
      column(width = 4,
             numericInput(NS(id,"maxFrcAct"), label = div(style = "font-weight: normal; font-style: italic", "Maximum (%):"), value = 25, min = 0)
      ),
      column(width = 4,
             numericInput(NS(id,"numbins"), label = div(style = "font-weight: normal; font-style: italic", "Simulation intervals"), value = 10, min = 1)
      )
    ),
    fluidRow(
      div(id=NS(id,"info"), verbatimTextOutput(NS(id,"info")))
    )
  )
}

step2_server <- function(id, info = FALSE){
  moduleServer(id, function(input, output, session){
    shinyjs::toggleElement("info", condition = info)

    observe({
      minFrcAct <- input$minFrcAct
      maxFrcAct <- input$maxFrcAct
      updateNumericInput(session, "minFrcAct", max = maxFrcAct)
      updateNumericInput(session, "maxFrcAct", min = minFrcAct)
    })

    out <- reactiveValues()

    observe({
      out$minFrcAct <- input$minFrcAct/100
      out$maxFrcAct <- input$maxFrcAct/100
      out$numbins <- input$numbins
    })

    output$info <- renderPrint(reactiveValuesToList(out))
    out
  })
}

