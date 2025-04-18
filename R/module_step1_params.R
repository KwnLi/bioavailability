step1_input <- function(id){
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      div(
        id = NS(id,"sampSpace"),
        column(width = 4,
          numericInput(NS(id,"sampmax"), label = "Maximum number of samples to simulate:", value = 10, min = 2),
          textInput(NS(id,"incr.vec"), "List of composite increment numbers to test, separated by commas (1=discrete sampling)")
        ),
      ),
      column(width = 4,
        radioButtons(NS(id,"abs_frcAct"), "Assumed level of soil contamination to simulate (expressed in terms of %)",
                     choices = c(`+/-25% action level` = 0.25, "Custom"),
                     selected = 0.25)
      ),
      column(width = 4,
        # conditional input if fraction above/below is custom
        div(
          id = NS(id,"inputCustom"),
          numericInput(NS(id,"frcAct_custom"),
                       label = div(style = "font-weight: normal; font-style: italic",
                                   "*Custom value (%) (enter a neg. (-) value to run a false exceedance error simulation or pos. (+) value for false compliance error simulation):"),
                       30, step = 1, min = -Inf, max = Inf)
        )
      )
    ),
    fluidRow(
      div(id=NS(id,"info"), verbatimTextOutput(NS(id,"info")))
    )
  )
}

step1_server <- function(id, testIncr = FALSE, info=FALSE){
  moduleServer(id, function(input, output, session){
    shinyjs::toggleElement("info", condition = info)

    out <- reactiveValues()

    # show custom input if custom is selected
    observe({

      if(testIncr){
        shinyjs::showElement(id="sampSpace")
        out$sampmax <- input$sampmax
        out$incr.vec <- as.numeric(strsplit(input$incr.vec, split = ",")[[1]])
      }else{
        shinyjs::hideElement(id="sampSpace")
      }

      if(input$abs_frcAct=="Custom"){
        shinyjs::showElement(id="inputCustom")
        out$frcAct <-input$frcAct_custom/100
      }else{
        shinyjs::hideElement(id="inputCustom")
        out$frcAct <- as.numeric(input$abs_frcAct)
      }
    })

    output$info <- renderPrint(reactiveValuesToList(out))
    out
  })
}

