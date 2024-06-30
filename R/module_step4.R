# Step 4 interface ---------------

step4_interface <- function(id){
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(width = 12,
             h3("Sampling Protocol"),
             samples_input(NS(id,"sample_params"))
      )
    ),
    hr(),
    fluidRow(
      column(width = 12,
             h3("Step 4 Input"),
             step3_input(NS(id,"step4_params")) # use step 3 module because it's the same
      )
    ),
    hr(),
    fluidRow(
      column(width = 12,
             h3("Decision Unit Assumptions"),
             du_assum_input(NS(id,"du_params"))
      )
    ),
    hr(),
    fluidRow(
      column(width = 12,
             h3("Advanced Settings: Simulation Parameters"),
             sim_params_input(id=NS(id,"sim_params"))
      )
    ),
    hr(),
    fluidRow(
      id=NS(id,"info"),
      verbatimTextOutput(NS(id,"info"))
    )
  )
}

step4_interface_server <- function(id, contam, info=FALSE){

  moduleServer(id, function(input, output, session){
    shinyjs::toggleElement("info", condition = info)

    # collect Step 4 inputs
    inputs <- reactiveValues()

    observe({
      inputs$contam <- contam
    })

    inputs$sample_params <- samples_server("sample_params", inputn = FALSE)
    inputs$step4_params <- step3_server("step4_params") # use step 3 module because it's the same
    inputs$du_params <- du_assum_server("du_params", show.dist.params=FALSE)
    inputs$sim_params <- sim_params_server("sim_params")

    # flatten into one list
    out <- reactive({
      unlist(lapply(unname(reactiveValuesToList(inputs)),
                    reactiveValuesToList),
             recursive = FALSE)
    })

    output$info <-renderPrint({out()})

    out
  })
}

# Step 4 run simulation -------------

step4_run <- function(id){
  tagList(
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    shinyWidgets::actionBttn(
      inputId = NS(id,"runStep4"),
      label ="Run simulation", style = "pill", color = "success"
    )
  )
}

step4_run_server <- function(id, step4_params){
  moduleServer(id, function(input,output,session){

    observe({
      tot.present <- length(step4_params()$meas.tot)>0
      ivba.present <- length(step4_params()$meas.ivba)>0
      shinyjs::toggleState("runStep4", condition = all(tot.present, ivba.present))
    })

    results <- reactiveValues()

    out <- reactive({
      shinyWidgets::progressSweetAlert(
        session = session, id = "step4_progress",
        title = "Running step 4 simulations", value = 10
      )

      params_step4 <- c(step4_params(), useMeanTot = TRUE)

      step4result <- do.call(step4, params_step4)

      shinyWidgets::updateProgressBar(session = session,
                                      title = "Running step 4 simulations",
                                      id = "step4_progress", value = 90)

      shinyWidgets::closeSweetAlert(session = session)

      shinyWidgets::sendSweetAlert(
        session = session,
        title =" Step 4 simulation completed",
        type = "success"
      )

      c(step4result, AsPb = params_step4$AsPb)

    }) |> bindEvent(input$runStep4)

    out
  })
}

# Step 4 results------------

step4_results <- function(id){
  tagList(
    tableOutput(NS(id,"simTable")),
    br(),
    tableOutput(NS(id,"simIntTable")),
    br(),
    htmlOutput(NS(id,"simIntTableText")),
    br(),
    plotOutput(NS(id,"step4Plot"), width = "860px"),
    br(),
    htmlOutput(NS(id,"accuracyText")),
    br(),
    htmlOutput(NS(id,"precisionText")),
    br(),
    tool_notes(),
    br(),
    br(),
    tool_disclaimer()
  )
}

step4_results_server <- function(id, step4_output){
  moduleServer(id, function(input,output,session){

    output$simTable <- renderTable({
      data.frame(`Model input` = c(
        paste0("Measured EPC (mg bioavailable ", step4_output()$AsPb, " per kg)"),
        "Measured EPC (% above/below the AL)",
        paste0("CoV in total ", step4_output()$AsPb, " across the DU "),
        "CoV in % RBA across the DU",
        "Estimated mean % RBA"),
        `Value inferred post-sampling based on sampling results` =
          c(step4_output()$step3$meas.ba,
            100*step4_output()$step3$meas.frcAct,
            step4_output()$step3$coeV.tot,
            step4_output()$step3$coeV.rba,
            step4_output()$step3$mn.rba),
        check.names = FALSE
      )
    }, width = "860px")

    output$simIntTable <- renderTable({
      data.frame(`Intermediete values used to derive updated model inputs` =
                   c(paste0("S.D. in total ", step4_output()$AsPb, " across composites**"),
                     "S.D. in % RBA across composites*"),
                 `Value inferred post-sampling based on sampling results` =
                   c(step4_output()$step3$sd.tot,
                     step4_output()$step3$sd.rba),
                 check.names = FALSE
      )
    }, width = "860px")

    output$simIntTableText <- renderText({
      paste0(
        "** S.D. observed across X composites converted to CoV in total ",
        step4_output()$AsPb,
        " or % RBA using the following equation: S.D. (sample increments )= S.D. (observed across N composites ) x  √(# increments)")
    })

    step4plotdata <- reactive({
      step4_plot(step4_output())
    })

    output$step4Plot <- renderPlot({
      step4plotdata()$outplot
    })

    output$accuracyText <- renderUI({
      HTML(step4plotdata()$accuracyText)
    })
    output$precisionText <- renderUI({
      HTML(step4plotdata()$precisionText)
    })

  })
}
