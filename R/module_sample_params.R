contaminant_input <- function(id){
  tagList(
    shinyWidgets::radioGroupButtons(NS(id,"AsPb"),
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
    fluidRow(
      div(
        id = NS(id, "sampleParam_tot"),
        column(
          width = 4,
          numericInput(NS(id,"tot.n"), "# of samples to be analyzed for total metal concentration", 1, min = 1)
          )
        ),
      div(
        id = NS(id, "compositeParam_tot"),
        column(width = 4,
               radioButtons(NS(id,"composite_tot"), "Total metal concentration sample aggregation", choices = c(Discrete = FALSE, Composite = TRUE))
               )
        ),
      div(
        id = NS(id, "incrParam_tot"),
        column(width = 4,
               numericInput(NS(id,"incr_tot"),
                            label = div(style = "font-weight: normal; font-style: italic",
                                        "*Increments per total composite sample:"),
                            value=2, min = 2)
        )
      )
    ),

    # IVBA sample input
    fluidRow(
      div(
        id = NS(id, "sampleParam_ivba"),
        column(
          width = 4,
          numericInput(NS(id,"ivba.n"), "# of samples to be analyzed for IVBA", 1, min = 1)
          )
        ),
      div(
        id = NS(id, "compositeParam_ivba"),
        column(width = 4,
               radioButtons(NS(id,"composite_ivba"), "IVBA sample aggregation", choices = c(Discrete = FALSE, Composite = TRUE))
               )
        ),
      div(
        id = NS(id, "incrParam_ivba"),
        column(width = 4,
               numericInput(NS(id,"incr_ivba"),
                            label = div(style = "font-weight: normal; font-style: italic",
                                        "*Increments per IVBA composite sample:"),
                            value=2, min = 2)
        )
      )
    ),

    div(id="info", verbatimTextOutput(NS(id,"info")))
  )
}

samples_server <- function(id, inputn=TRUE, askComposite=TRUE, info=FALSE){
  moduleServer(id, function(input, output, session){
    shinyjs::toggle("sampleParam_tot", condition=inputn)
    shinyjs::toggle("sampleParam_ivba", condition=inputn)
    shinyjs::toggle("compositeParam_tot", condition=askComposite)
    shinyjs::toggle("compositeParam_ivba", condition=askComposite)
    shinyjs::toggleElement("info", condition = info)

    observe({
      # show/hide total composite options
      comp.choice.tot <- input$composite_tot
      if(comp.choice.tot){
        updateNumericInput(session, "incr_tot", value = 2)
        shinyjs::showElement("incrParam_tot")
      }else{
        updateNumericInput(session, "incr_tot", value = 1)
        shinyjs::hideElement("incrParam_tot")
      }

      # show/hide ivba composite options
      comp.choice.ivba <- input$composite_ivba
      if(comp.choice.ivba){
        updateNumericInput(session, "incr_ivba", value = 2)
        shinyjs::showElement("incrParam_ivba")
      }else{
        updateNumericInput(session, "incr_ivba", value = 1)
        shinyjs::hideElement("incrParam_ivba")
      }
    })

    out <- reactiveValues()

    observe({
      if(inputn){
        out$tot.n <- input$tot.n
        out$ivba.n <- input$ivba.n
      }

      if(askComposite){
        out$tot.incr <- input$incr_tot
        out$ivba.incr <- input$incr_ivba
      }
    })

    output$info <- renderPrint(reactiveValuesToList(out))

    out
  })
}


