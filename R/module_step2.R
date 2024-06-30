# Step 2 interface ---------------

step2_interface <- function(id){
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
             h3("Step 2 Input"),
             step2_input(NS(id,"step2_params"))
      )
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
      verbatimTextOutput(NS(id,"info"))
    )
  )
}

step2_interface_server <- function(id, contam, info=FALSE){

  moduleServer(id, function(input, output, session){
    shinyjs::toggleElement("info", condition = info)

    # collect step 2 inputs
    inputs <- reactiveValues()

    observe({
      inputs$contam <- contam
    })

    inputs$sample_params <- samples_server("sample_params")
    inputs$step2_params <- step2_server("step2_params")
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

# Step 2 run simulation -------------

step2_run <- function(id){
  tagList(
    shinyWidgets::useSweetAlert(),
    shinyWidgets::actionBttn(
      inputId = NS(id,"runStep2"),
      label ="Run simulation", style = "pill", color = "success"
    )
  )
}

step2_run_server <- function(id, step2_params){
  moduleServer(id, function(input,output,session){

    results <- reactiveValues()

    observe({
      shinyWidgets::progressSweetAlert(
        session = session, id = "step2_progress",
        title = "Running step 2 simulations: False compliance", value = 10
      )

      params_type1 <- c(step2_params()$params, useMeanTot = TRUE, useMeanIVBA = TRUE)

      step2result_type1 <- do.call(step2, params_type1)

      shinyWidgets::updateProgressBar(session = session,
                                      title = "Running step 2 simulations: False exceedance",
                                      id = "step2_progress", value = 50)

      params_type2 <- c(step2_params()$params, useMeanTot = TRUE, useMeanIVBA = TRUE)
      params_type2$minFrcAct <- -params_type2$minFrcAct
      params_type2$maxFrcAct <- -params_type2$maxFrcAct

      step2result_type2 <- do.call(step2, params_type2)

      results$type1 <- step2result_type1
      results$type2 <- step2result_type2

      shinyWidgets::updateProgressBar(session = session,
                                      title = "Making outputs",
                                      id = "step2_progress", value = 90)

      # Make outputs
      s2_t1 <- step2_plot(results$type1, error_threshold = step2_params()$decision_obj$type1error)
      s2_t2 <- step2_plot(results$type2, error_threshold = step2_params()$decision_obj$type2error)

      results$step2Plot <- cowplot::plot_grid(s2_t1, s2_t2)

      results$step2TextType1 <- HTML(step2_text_t1(step2result_type1, error_threshold = step2_params()$decision_obj$type1error))
      results$step2TextType2 <- HTML(step2_text_t2(step2result_type2, error_threshold = step2_params()$decision_obj$type2error))

      shinyWidgets::closeSweetAlert(session = session)
      shinyWidgets::sendSweetAlert(
        session = session,
        title =" Step 2 simulation completed",
        type = "success"
      )

    }) |> bindEvent(input$runStep2)

    # flatten into one list
    out <- reactive({
      reactiveValuesToList(results)
    })

    out
  })
}

# Step 2 results------------

step2_results <- function(id){
  tagList(
    plotOutput(NS(id,"step2Plot"), width = "860px"),
    br(),
    htmlOutput(NS(id,"step2TextType1")),
    br(),
    htmlOutput(NS(id,"step2TextType2")),
    br(),
    tool_notes(),
    br(),
    br(),
    tool_disclaimer()
  )
}

step2_results_server <- function(id, step2_output){
  moduleServer(id, function(input,output,session){
    output$step2Plot <- renderPlot({
      step2_output()$step2Plot
    })

    output$step2TextType1 <- renderUI({
      step2_output()$step2TextType1
    })

    output$step2TextType2 <- renderUI({
      step2_output()$step2TextType2
    })

  })
}
