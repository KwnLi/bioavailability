# Step 1a interface ---------------

step1a_interface <- function(id){
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(width=6,
             h3("Sampling protocol"),
             samples_input(NS(id,"sample_params"))),
      column(width=6,
             h3("Step 1a input"),
             step1_input(NS(id,"step1a_params")))
    ),
    fluidRow(
      column(width=6,
             h3("Decision unit assumptions"),
             du_assum_input(NS(id,"du_params"))),
      column(width=6,
             h3("Simulation parameters"),
             sim_params_input(id=NS(id,"sim_params")))
    ),
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

# Step 1a output -------------

step1a_output <- function(id){
  tagList(
    shinyWidgets::useSweetAlert(),
    actionButton(
      inputId = NS(id,"runStep1a"),
      label ="Run simulation"
    )
  )
}

step1a_output_server <- function(id, step1a_params){
  moduleServer(id, function(input,output,session){

    out <- reactiveValues()

    observe({
      progressSweetAlert(
        session = session, id = "step1a_progress",
        title = "Running step 1A simulations", value = 0
        )

      params_above <- c(step1a_params(), useMeanTot = TRUE, useMeanIVBA = TRUE, outputLvl = 2)

      step1aresult_above <- do.call(simDU, params_above)
      #   simDU(
      #   AsPb = input$AsPb,
      #   actLvl = input$actLvl,
      #   actLvlRBA = input$actLvlRBA,
      #   tot.n = as.numeric(input$tot.n),
      #   ivba.n = as.numeric(input$ivba.n),
      #   tot.incr = as.numeric(input$incr),
      #   ivba.incr = as.numeric(input$incr),
      #   useMeanTot = TRUE,
      #   useMeanIVBA = TRUE,
      #   frcAct = abs_frcAct(),
      #   coeV.tot = coeV_tot(),
      #   coeV.rba = coeV_rba(),
      #   mn.rba = mn_rba(),
      #   error_tot = as.logical(input$error_tot),
      #   error_ivb = as.logical(input$error_ivb),
      #   error_ivb_cv = as.numeric(input$error_ivb_cv),
      #   ivba_model = as.logical(input$ivba_model),
      #   post_mean = as.logical(input$post_mean),
      #   iter = input$iter,
      #   dist_tot = input$dist_tot,
      #   dist_rba = input$dist_rba,
      #   outputLvl = 2
      # )

      updateProgressBar(session = session, id = "step1a_progress", value = 50)

      params_below <- c(step1a_params(), useMeanTot = TRUE, useMeanIVBA = TRUE, outputLvl = 2)
      params_below$frcAct <- -params_below$frcAct

      step1aresult_below <- do.call(simDU, params_below)
      #   simDU(
      #   AsPb = input$AsPb,
      #   actLvl = input$actLvl,
      #   actLvlRBA = input$actLvlRBA,
      #   tot.n = as.numeric(input$tot.n),
      #   ivba.n = as.numeric(input$ivba.n),
      #   tot.incr = as.numeric(input$incr),
      #   ivba.incr = as.numeric(input$incr),
      #   useMeanTot = TRUE,
      #   useMeanIVBA = TRUE,
      #   frcAct = -abs_frcAct(),
      #   coeV.tot = coeV_tot(),
      #   coeV.rba = coeV_rba(),
      #   mn.rba = mn_rba(),
      #   error_tot = as.logical(input$error_tot),
      #   error_ivb = as.logical(input$error_ivb),
      #   error_ivb_cv = as.numeric(input$error_ivb_cv),
      #   ivba_model = as.logical(input$ivba_model),
      #   post_mean = as.logical(input$post_mean),
      #   iter = input$iter,
      #   dist_tot = input$dist_tot,
      #   dist_rba = input$dist_rba,
      #   outputLvl = 2
      # )

      out$above <- step1aresult_above
      out$below <- step1aresult_below
      out$viz_above <- step1a_plot(step1aresult_above)
      out$viz_below <- step1a_plot(step1aresult_below)

      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title =" Step 1a simulation completed",
        type = "success"
      )

    }) |> bindEvent(input$runStep1a)

    out
  })
}
