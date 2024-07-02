devtools::load_all()

ui <- tagList(
  tags$style("html,body{background-color: white;}
                .container{
                    width: 100%;
                    margin: 0 auto;
                    padding: 0;
                }
               @media screen and (min-width: 700px){
                .container{
                    min-width: 1150px;
                }
               }
             "),
  tags$div(
    class="container",
    dashboardPage(
      dashboardHeader(title="Bioavailability Sample Planning & Evaluation Tool",
                      titleWidth = 450),
      dashboardSidebar(
        sidebarMenu(
          contaminant_input("contam"),
          hr(),
          menuItem("Step 1A", tabName = "step1a"),
          menuItem("Step 1B", tabName = "step1b"),
          menuItem("Step 2", tabName = "step2"),
          menuItem("Step 3", tabName = "step3"),
          menuItem("Step 4", tabName = "step4")
        )
      ),
      dashboardBody(
        tags$head(tags$style(HTML(".sidebar-toggle {display: none;}"))),

        tabItems(
          tabItem(tabName = "step1a",
                  fluidRow(
                    tabBox(width=12,
                           title = "Step 1A: Evaluate single sampling protocol (pre-sampling)",
                           id = "step1a_box",
                           side="right", selected = "Parameter input",
                           tabPanel(
                             title = "Output",
                             step1a_results("step1a_results"),
                             br(),
                             div(
                               style="display:inline-block;margin-left: 40%;padding-bottom: 10px;",
                               make_pdf_ui("step1a_pdf")
                             )
                           ),
                           tabPanel(
                             title = "Parameter input",
                             step1a_interface("step1a_ui"),
                             div(
                               style="display:inline-block;margin-left: 40%;padding-bottom: 10px;",
                               step1a_run("step1a_run")
                             )
                           )
                    )
                  )
          ),
          tabItem(tabName = "step1b",
                  fluidRow(
                    tabBox(width=12,
                           title = "Step 1B: Evaluate multiple sampling protocols (pre-sampling)",
                           id = "step1b_box",
                           side="right", selected = "Parameter input",
                           tabPanel(
                             title = "Output",
                             step1b_results("step1b_results")
                           ),
                           tabPanel(
                             title = "Parameter input",
                             step1b_interface("step1b_ui"),
                             div(
                               style="display:inline-block;margin-left: 40%;padding-bottom: 10px;",
                               step1b_run("step1b_run")
                             )
                           )
                    )
                  )
          ),
          tabItem(tabName = "step2",
                  fluidRow(
                    tabBox(width=12,
                           title = "Step 2: Evaluate decision tolerance of preferred sampling protocol (pre-sampling)",
                           id = "step2_box",
                           side="right", selected = "Parameter input",
                           tabPanel(
                             title = "Output",
                             step2_results("step2_results")
                           ),
                           tabPanel(
                             title = "Parameter input",
                             step2_interface("step2_ui"),
                             div(
                               style="display:inline-block;margin-left: 40%;padding-bottom: 10px;",
                               step2_run("step2_run")
                             )
                           )
                    )
                  )
          ),
          tabItem(tabName = "step3",
                  fluidRow(
                    tabBox(width=12,
                           title = "Step 3: Evaluate simulation inputs (post-sampling)",
                           id = "step3_box",
                           side="right", selected = "Parameter input",
                           tabPanel(
                             title = "Output",
                             step3_results("step3_results")
                           ),
                           tabPanel(
                             title = "Parameter input",
                             step3_interface("step3_ui"),
                             div(
                               style="display:inline-block;margin-left: 40%;padding-bottom: 10px;",
                               step3_run("step3_run")
                             )
                           )
                    )
                  )
          ),
          tabItem(tabName = "step4",
                  fluidRow(
                    tabBox(width=12,
                           title = "Step 4: Estimate decision accuracy and precision (post-sampling)",
                           id = "step4_box",
                           side="right", selected = "Parameter input",
                           tabPanel(
                             title = "Output",
                             step4_results("step4_results")
                           ),
                           tabPanel(
                             title = "Parameter input",
                             step4_interface("step4_ui"),
                             div(
                               style="display:inline-block;margin-left: 40%;padding-bottom: 10px;",
                               step4_run("step4_run")
                             )
                           )
                    )
                  )
          )
        ),
        tags$head(
          tags$style(
            "
          body{
              height: auto;
              margin: auto;
              overflow-x: auto;
          }"
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  contam <- contaminant_server("contam")

  session.tempdir <- tempfile(pattern = "tmp", tmpdir = "temp")
  dir.create(session.tempdir)
  onStop(function() unlink(session.tempdir, recursive = TRUE))

  # STEP 1A
  step1a_params <- step1a_interface_server("step1a_ui", contam = contam)
  step1a_output <- step1a_run_server("step1a_run", step1a_params = step1a_params)
  step1a_results_server("step1a_results", step1a_output)

  make_pdf_server("step1a_pdf",
                  template.path = "templates/report_step1a.Rmd",
                  temp.dir = session.tempdir,
                  report.params = list(viz_above = step1a_output()$viz_above,
                                       viz_below = step1a_output()$viz_below))

  observe({
    updateTabsetPanel(session=session,
                      inputId = "step1a_box",
                      selected = "Output"
                      )
  }) |> bindEvent(step1a_output(), ignoreInit = TRUE)

  # STEP 1B
  step1b_params <- step1b_interface_server("step1b_ui", contam = contam, info = FALSE)
  step1b_output <- step1b_run_server("step1b_run", step1b_params = step1b_params)
  step1b_results_server("step1b_results", step1b_output)

  observe({
    updateTabsetPanel(session=session,
                      inputId = "step1b_box",
                      selected = "Output"
    )
  }) |> bindEvent(step1b_output(), ignoreInit = TRUE)

  # STEP 2
  step2_params <- step2_interface_server("step2_ui", contam = contam, info = FALSE)
  step2_output <- step2_run_server("step2_run", step2_params = step2_params)
  step2_results_server("step2_results", step2_output)

  observe({
    updateTabsetPanel(session=session,
                      inputId = "step2_box",
                      selected = "Output"
    )
  }) |> bindEvent(step2_output(), ignoreInit = TRUE)

  # STEP 3
  step3_params <- step3_interface_server("step3_ui", contam = contam, info = FALSE)
  step3_output <- step3_run_server("step3_run", step3_params = step3_params)
  step3_results_server("step3_results", step3_output)

  observe({
    updateTabsetPanel(session=session,
                      inputId = "step3_box",
                      selected = "Output"
    )
  }) |> bindEvent(step3_output())

  # STEP 4
  step4_params <- step4_interface_server("step4_ui", contam = contam, info = FALSE)
  step4_output <- step4_run_server("step4_run", step4_params = step4_params)
  step4_results_server("step4_results", step4_output)

  observe({
    updateTabsetPanel(session=session,
                      inputId = "step4_box",
                      selected = "Output"
    )
  }) |> bindEvent(step4_output())

}

shinyApp(ui, server)
