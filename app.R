devtools::load_all()

ui <- bslib::page_sidebar(
  title = "Sitewide RBA",
  sidebar =   bslib::sidebar(
    width = 300, open = NA,
    shinyjs::useShinyjs(),
    bslib::accordion(
      open = c("Sampling plan", "Site parameters", "DU parameters"),
      bslib::accordion_panel(
        "Sampling plan",
        shinyjs::useShinyjs(),
        shiny::numericInput("DU.n", "Number of decision units in the site", min = 1, value = 15),
        shiny::numericInput("ivba.incr",
                            label = div(style = "font-weight: normal; font-style: italic",
                                        "*Increments per IVBA composite sample:"),
                            value=1, min = 1)
      ),
      bslib::accordion_panel(
        "Site parameters",
        shiny::selectInput("simDist_rba_site", "Site RBA distribution:",
                           choices = c(`truncated normal` = "truncnorm",
                                       `truncated log-normal` = "trunclognorm",
                                       normal = "normal",
                                       `log-normal` = "lognorm"),
                           selected = "truncnorm"),
        shiny::numericInput("mn_rba_site", "True RBAsite mean", value = 60),
        shiny::radioButtons("coeV_rba_site", "Sitewide RBA coefficient of variance (CoV):",
                            choices = c(0.5, 1, 3, "Custom"), inline=TRUE),
        shiny::div(
          id = "input_coeV_rba_site_custom",
          shiny::numericInput("coeV_rba_site_custom",
                              label = div(style = "font-weight: normal; font-style: italic", "[Custom site RBA CoV value]"),
                              0.75, step = 0.05, min = 0)
        )
      ),
      bslib::accordion_panel(
        "Advanced simulation parameters",
        shiny::numericInput("error_ivb_cv", "IVBA model error coefficient of variance", 0.05, min = 0, max = 1, step = 0.01),
        shiny::numericInput("iter", "Number of simulation iterations", 5000, min = 10)
      )
    ),
    shiny::actionButton("Run", label = "Run Simulation")
  ),
  download_interface("download")
)

server <- function(input, output, session) {

  # create temp directory
  session.tempdir <- tempfile(pattern = "tmp", tmpdir = "temp")
  dir.create(session.tempdir, recursive = TRUE)
  onStop(function() unlink(session.tempdir, recursive = TRUE))

  # parameter input
  params <- shiny::reactiveValues()

  shiny::observe({
    params$DU.n <- input$DU.n
    params$mn_rba_site <- input$mn_rba_site
    params$simDist_rba_site <- input$simDist_rba_site
    params$ivba.incr <- input$ivba.incr
    params$error_ivb_cv <- input$error_ivb_cv
    params$iter <- input$iter

    if(input$coeV_rba_site=="Custom"){
      shinyjs::showElement("input_coeV_rba_site_custom")
      params$coeV_rba_site <- input$coeV_rba_site_custom
    }else{
      shinyjs::hideElement("input_coeV_rba_site_custom")
      params$coeV_rba_site <- as.numeric(input$coeV_rba_site)
    }
  })

  shiny::observe({
    shinyalert::shinyalert("Simulation running", "Simulation in progress, please wait",
                           closeOnEsc = FALSE, closeOnClickOutside = FALSE,
                           showConfirmButton = FALSE, animation = FALSE)

    result <- reactiveVal()
    input.params <- reactiveValuesToList(params)
    output$info <- renderPrint({input.params})
    result(do.call(simSite, input.params))
    shinyalert::closeAlert()
    download_server("download", result,
                    tmpdir = session.tempdir,
                    stepdirname = "site",
                    default.downloadname = "sitewide_simdata",
                    unlist_step_output = FALSE)
  }) |>
    bindEvent(input$Run)

  # output$output <- renderPrint({results})

}

shinyApp(ui, server)
