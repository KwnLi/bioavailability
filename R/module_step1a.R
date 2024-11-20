# Step 1a input ---------------

step1a_interface <- function(id){
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
        h3("Step 1A Input"),
        step1_input(NS(id,"step1a_params"))
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

step1a_interface_server <- function(id, contam, info=FALSE){

  moduleServer(id, function(input, output, session){
    shinyjs::toggleElement("info", condition = info)

    # collect step 1a inputs
    inputs <- reactiveValues()

    observe({
      inputs$contam <- contam
    })

    inputs$sample_params <- samples_server("sample_params")
    inputs$step1a_params <- step1_server("step1a_params", testIncr = FALSE)
    inputs$du_params <- du_assum_server("du_params")
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

# Step 1a run simulation -------------

step1a_run <- function(id){
  tagList(
    shinyWidgets::useSweetAlert(),
    shinyWidgets::actionBttn(
      inputId = NS(id,"runStep1a"),
      label ="Run simulation", style = "pill", color = "success"
    )
  )
}

step1a_run_server <- function(id, step1a_params){
  moduleServer(id, function(input,output,session){

    results <- reactiveValues()

    observe({
      shinyWidgets::progressSweetAlert(
        session = session, id = "step1a_progress",
        title = "Running step 1A simulations: False compliance", value = 10
        )

      params_above <- c(step1a_params(), useMeanTot = TRUE, useMeanIVBA = TRUE, outputLvl = 4)

      step1aresult_above <- do.call(simDU, params_above)

      shinyWidgets::updateProgressBar(session = session,
                                      title = "Running step 1A simulations: False exceedance",
                                      id = "step1a_progress", value = 50)

      params_below <- c(step1a_params(), useMeanTot = TRUE, useMeanIVBA = TRUE, outputLvl = 4)
      params_below$frcAct <- -params_below$frcAct

      step1aresult_below <- do.call(simDU, params_below)

      results$above <- step1aresult_above
      results$below <- step1aresult_below

      shinyWidgets::updateProgressBar(session = session,
                                      title = "Making outputs",
                                      id = "step1a_progress", value = 90)

      results$viz_above <- step1a_plot(step1aresult_above)
      results$viz_below <- step1a_plot(step1aresult_below)

      shinyWidgets::closeSweetAlert(session = session)
      shinyWidgets::sendSweetAlert(
        session = session,
        title =" Step 1A simulation completed",
        type = "success"
      )

    }) |> bindEvent(input$runStep1a)

    # flatten into one list
    out <- reactive({
      reactiveValuesToList(results)
    })

    out
  })
}

# Step 1a results------------

step1a_results <- function(id){
  tagList(
    plotOutput(NS(id,"step1aPlot"), width = "860px"),
    br(),
    htmlOutput(NS(id,"step1aText")),
    br(),
    tool_notes(),
    br(),
    br(),
    tool_disclaimer()
  )
}

step1a_results_server <- function(id, step1a_output){
  moduleServer(id, function(input,output,session){

    output$step1aPlot <- renderPlot({
      cowplot::plot_grid(step1a_output()$viz_above[[1]], step1a_output()$viz_below[[1]])
    })

    output$step1aText <- renderUI({
      HTML(paste(step1a_output()$viz_above[[2]],
                 step1a_output()$viz_below[[2]],
                 sep = "<br/><br/>"))
    })

  })
}
