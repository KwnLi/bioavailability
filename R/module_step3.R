# Step 3 interface ---------------

step3_interface <- function(id){
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
             h3("Step 3 Input"),
             step3_input(NS(id,"step3_params"))
      )
    ),
    hr(),
    fluidRow(
      id=NS(id,"info"),
      verbatimTextOutput(NS(id,"info"))
    )
  )
}

step3_interface_server <- function(id, contam, info=FALSE){

  moduleServer(id, function(input, output, session){
    shinyjs::toggleElement("info", condition = info)

    # collect Step 3 inputs
    inputs <- reactiveValues()

    observe({
      inputs$contam <- contam
    })

    inputs$sample_params <- samples_server("sample_params", inputn = FALSE)
    inputs$step3_params <- step3_server("step3_params")

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

# Step 3 run simulation -------------

step3_run <- function(id){
  tagList(
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    shinyWidgets::actionBttn(
      inputId = NS(id,"runStep3"),
      label ="Run simulation", style = "pill", color = "success"
    )
  )
}

step3_run_server <- function(id, step3_params){
  moduleServer(id, function(input,output,session){

    observe({
      tot.present <- length(step3_params()$meas.tot)>0
      ivba.present <- length(step3_params()$meas.ivba)>0
      shinyjs::toggleState("runStep3", condition = all(tot.present, ivba.present))
    })

    results <- reactiveValues()

    out <- reactive({
      shinyWidgets::progressSweetAlert(
        session = session, id = "step3_progress",
        title = "Running step 3 simulations", value = 10
      )

      params_step3 <- c(step3_params(), useMeanTot = TRUE)

      step3result <- do.call(step3, params_step3)

      shinyWidgets::updateProgressBar(session = session,
                                      title = "Running step 3 simulations",
                                      id = "step3_progress", value = 90)

      shinyWidgets::closeSweetAlert(session = session)

      shinyWidgets::sendSweetAlert(
        session = session,
        title =" Step 3 simulation completed",
        type = "success"
      )

      c(step3result, AsPb = params_step3$AsPb)

    }) |> bindEvent(input$runStep3)

    out
  })
}

# Step 3 results------------

step3_results <- function(id){
  tagList(
    tableOutput(NS(id,"simTable")),
    br(),
    tableOutput(NS(id,"simIntTable")),
    br(),
    htmlOutput(NS(id,"simIntTableText")),
    br(),
    tool_notes(),
    br(),
    br(),
    tool_disclaimer()
  )
}

step3_results_server <- function(id, step3_output){
  moduleServer(id, function(input,output,session){

    output$simTable <- renderTable({
      data.frame(`Model input` = c(
        paste0("Measured EPC (mg bioavailable ", step3_output()$AsPb, " per kg)"),
        "Measured EPC (% above/below the AL)",
        paste0("CoV in total ", step3_output()$AsPb, " across the DU "),
        "CoV in % RBA across the DU",
        "Estimated mean % RBA"),
        `Value inferred post-sampling based on sampling results` =
          c(step3_output()$step3$meas.ba,
            100*step3_output()$step3$meas.frcAct,
            step3_output()$step3$coeV.tot,
            step3_output()$step3$coeV.rba,
            step3_output()$step3$mn.rba),
        check.names = FALSE
      )
    }, width = "860px")

    output$simIntTable <- renderTable({
      data.frame(`Intermediete values used to derive updated model inputs` =
                   c(paste0("S.D. in total ", step3_output()$AsPb, " across composites**"),
                     "S.D. in % RBA across composites*"),
                 `Value inferred post-sampling based on sampling results` =
                   c(step3_output()$step3$sd.tot,
                     step3_output()$step3$sd.rba),
                 check.names = FALSE
      )
    }, width = "860px")

    output$simIntTableText <- renderText({
      paste0(
        "** S.D. observed across X composites converted to CoV in total ",
        step3_output()$AsPb,
        " or % RBA using the following equation: S.D. (sample increments )= S.D. (observed across N composites ) x  √(# increments)")
    })

  })
}
