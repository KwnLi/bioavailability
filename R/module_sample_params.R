contaminant_input <- function(id){
  tagList(
    radioGroupButtons(NS(id,"AsPb"),
                 "Select contaminant",
                 choices = c(Pb = "Pb", As = "As")),
    numericInput(NS(id,"actLvl"),
                 "Enter site-specific action level (mg/kg):",
                 400, step = 10),
    numericInput(NS(id,"actLvlRBA"),
                 "Assumed RBA of the action level (%):",
                 60, step = 10, min = 0, max = 100)
  )
}

contaminant_server <- function(id){
  moduleServer(id, function(input,output,session){
    out <- reactiveValues()

    observe({
      out$AsPb <- input$AsPb
      out$actLvl <- input$actLvl
      out$actLvlRBA <- input$actLvlRBA
    })

    out
  })
}

samples_input <- function(id){
  tagList(
    shinyjs::useShinyjs(),

    # total concentration sample input
    div(
      id = NS(id, "sampleParam"),
      numericInput(NS(id,"tot.n"), "# of samples to be analyzed for total metal concentration", 1, min = 1),
      numericInput(NS(id,"ivba.n"), "# of samples to be analyzed for IVBA", 1, min = 1)
    ),

    # composite
    div(
      id = NS(id, "compositeParam"),
      radioButtons(NS(id,"composite"), "Sample aggregation", choices = c(Discrete = FALSE, Composite = TRUE), inline=TRUE),
      div(
        id = NS(id, "incrParam"),
        numericInput(NS(id,"incr"),
                     label = div(style = "font-weight: normal; font-style: italic",
                                 "*Increments per composite sample:"),
                     value=2, min = 2)
        ),
    ),
    div(id="info", verbatimTextOutput(NS(id,"info")))
  )
}

samples_server <- function(id, inputn=TRUE, askComposite=TRUE, info=TRUE){
  moduleServer(id, function(input, output, session){
    shinyjs::toggle("sampleParam", condition=inputn)
    shinyjs::toggle("compositeParam", condition=askComposite)
    shinyjs::toggle("info", condition=info)

    observe({
      comp.choice <- input$composite
      if(comp.choice==FALSE){
        updateNumericInput(session, "incr", value = 1)
        shinyjs::hideElement("incrParam")
      }else{
        updateNumericInput(session, "incr", value = 2)
        shinyjs::showElement("incrParam")
      }
    })

    out <- reactiveValues()

    observe({
      out$tot.n <- input$tot.n
      out$ivba.n <- input$ivba.n
      out$composite <- input$composite
      out$incr <- input$incr
    })

    output$info <- renderPrint(reactiveValuesToList(out))

    out
  })
}


