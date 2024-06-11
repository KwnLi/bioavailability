step1_input <- function(id){
  tagList(
    shinyjs::useShinyjs(),
    div(
      id = NS(id,"sampSpace"),
      numericInput(NS(id,"sampmax"), label = "Maximum number of samples to simulate:", value = 10, min = 2),
      textInput(NS(id,"incr.vec"), "List of composite increment numbers to test, separated by commas (1=discrete sampling)")
    ),
    radioButtons(NS(id,"abs_frcAct"), "Assumed level of soil contamination to simulate (expressed in terms of %)",
                 choices = c(`+/-25% action level` = 0.25, "Custom"),
                 selected = 0.25),
    # conditional input if fraction above/below is custom
    div(
      id = NS(id,"inputCustom"),
      numericInput(NS(id,"frcAct_custom"),
                   label = div(style = "font-weight: normal; font-style: italic",
                               "*Custom value (%) (enter a neg. (-) value to run a false exceedance error simulation or pos. (+) value for false compliance error simulation):"),
                   30, step = 1, min = -Inf, max = Inf)
      ),
    div(id=NS(id,"info"), verbatimTextOutput(NS(id,"info")))
  )
}

step1_server <- function(id, testIncr = FALSE){
  moduleServer(id, function(input, output, session){

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
        out$abs_frcAct <-input$frcAct_custom/100
      }else{
        shinyjs::hideElement(id="inputCustom")
        out$abs_frcAct <- as.numeric(input$abs_frcAct)
      }
    })

    output$info <- renderPrint(reactiveValuesToList(out))
    out
  })
}
