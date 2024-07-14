# Step 1b interface ---------------

step1b_interface <- function(id){
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(width=12,
             h3("Sampling Protocol"),
             samples_input(NS(id,"sample_params")))
      ),
    hr(),
    fluidRow(
      column(width=12,
             h3("Step 1B Input"),
             step1_input(NS(id,"step1b_params")))
      ),
    hr(),
    fluidRow(
      column(width=12,
             h3("Decision Error Probability Objective"),
             decision_obj_input(NS(id,"decision_obj"))
             )
      ),
    hr(),
    fluidRow(
      column(width=12,
             h3("Decision Unit Assumptions"),
             du_assum_input(NS(id,"du_params")))
      ),
    hr(),
    fluidRow(
      column(width=12,
             h3("Advanced Settings: Simulation Parameters"),
             sim_params_input(id=NS(id,"sim_params")))
    ),
    fluidRow(
      id=NS(id,"info"),
      verbatimTextOutput(NS(id,"info"))
    )
  )
}

step1b_interface_server <- function(id, contam, info=FALSE){

  moduleServer(id, function(input, output, session){
    shinyjs::toggleElement("info", condition = info)

    # collect step 1b inputs
    inputs <- reactiveValues()

    observe({
      inputs$contam <- contam
    })

    inputs$sample_params <- samples_server("sample_params", askComposite=FALSE)
    inputs$step1b_params <- step1_server("step1b_params", testIncr = TRUE)
    inputs$du_params <- du_assum_server("du_params")
    decision_obj <- decision_obj_server("decision_obj")
    inputs$sim_params <- sim_params_server("sim_params")

    # flatten into one list
    out <- reactive({
      list(
        params = unlist(lapply(unname(reactiveValuesToList(inputs)),
                               reactiveValuesToList),
                        recursive = FALSE),
        decision_obj = reactiveValuesToList(decision_obj)
      )
    })

    output$info <-renderPrint({out()})

    out
  })
}

# Step 1b run simulation -------------

step1b_run <- function(id){
  tagList(
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    shinyWidgets::actionBttn(
      inputId = NS(id,"runStep1b"),
      label ="Run simulation", style = "pill", color = "success"
    )
  )
}

step1b_run_server <- function(id, step1b_params){
  moduleServer(id, function(input,output,session){

    observe({
      incr.present <- length(step1b_params()$params$incr.vec)>0
      shinyjs::toggleState("runStep1b", condition = incr.present)
    })

    results <- reactiveValues()

    observe({
      shinyWidgets::progressSweetAlert(
        session = session, id = "step1b_progress",
        title = "Running step 1B simulations: False compliance", value = 10
      )

      # type 1 error
      params_type1 <- c(step1b_params()$params, useMeanTot = TRUE, useMeanIVBA = TRUE)

      step1bresults_type1 <- do.call(step1, params_type1)

      shinyWidgets::updateProgressBar(session = session,
                                      title = "Running step 1B simulations: False exceedance",
                                      id = "step1b_progress", value = 50)

      # type 2 error
      params_type2 <- c(step1b_params()$params, useMeanTot = TRUE, useMeanIVBA = TRUE)
      params_type2$frcAct <- -params_type2$frcAct

      step1bresult_type2 <- do.call(step1, params_type2)

      results$t1_error_threshold = step1b_params()$decision_obj$type1error
      results$t2_error_threshold = step1b_params()$decision_obj$type2error

      results$type1 <- step1bresults_type1
      results$type2 <- step1bresult_type2

      shinyWidgets::updateProgressBar(session = session,
                                      title = "Making outputs",
                                      id = "step1b_progress", value = 90)

      # plot output
      s1_t1 <- step1_plot(results$type1, error_threshold = results$t1_error_threshold)
      s1_t2 <- step1_plot(results$type2, error_threshold = results$t2_error_threshold)

      results$step1bPlot <- cowplot::plot_grid(s1_t1, s1_t2)

      # text output
      results$step1bText <- HTML(
        "**<i>X-axis sample number listed is based on the number of samples
        analyzed for total metal concentration, adding one additional sample
        incrementally (X+1). If the input number of samples analyzed for IVBA
        was different than that for totals, then the number of samples listed
        on the x-axis would not represent IVBA sample number. For example, if
        the sampling protocol originally input was 5 samples analyzed for totals
        and 3 samples analyzed for IVBA, then the left most data point would
        represent 5 samples for totals and 3 samples for IVBA, and each incremental
        data point would represent X+1 samples analyzed for totals and IVBA
        respectively.</i>"
      )

      shinyWidgets::closeSweetAlert(session = session)
      shinyWidgets::sendSweetAlert(
        session = session,
        title =" Step 1B simulation completed",
        type = "success"
      )

    }) |> bindEvent(input$runStep1b)

    # flatten into one list
    out <- reactive({
      reactiveValuesToList(results)
    })

    out
  })
}

# Step 1b results------------

step1b_results <- function(id){
  tagList(
    plotOutput(NS(id,"step1bPlot"), width = "860px"),
    br(),
    htmlOutput(NS(id,"step1bText")),
    br(),
    tool_notes(),
    br(),
    br(),
    tool_disclaimer()
  )
}

step1b_results_server <- function(id, step1b_output){
  moduleServer(id, function(input,output,session){
    output$step1bPlot <- renderPlot({
      step1b_output()$step1bPlot
    })

    output$step1bText <- renderUI({
      step1b_output()$step1bText
    })
  })
}
