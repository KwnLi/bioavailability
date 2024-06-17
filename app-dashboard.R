ui <- dashboardPage(
  dashboardHeader(title="Bioavailability Sample Planning & Evaluation Tool",
                  titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      contaminant_input("contam"),
      hr(),
      menuItem("Step 1a", tabName = "step1a"),
      menuItem("Step 1b", tabName = "step1b"),
      menuItem("Step 2", tabName = "step2"),
      menuItem("Step 3", tabName = "step3"),
      menuItem("Step 4", tabName = "step4")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "step1a",
              fluidRow(
                box(width=12,
                    title = "Step 1a Parameters",
                    step1a_interface("step1a_ui"),
                    step1a_output("step1a_run")
                    )
                )
              ),
      tabItem(tabName = "step1b",
              fluidRow(
                box(width=6,
                    step1b_samplesInput <- samples_input("samples_1b")
                ),
                box(width=6,
                    step1b_params <- step1_input("step1b_params"))
              )
      ),
      tabItem(tabName = "step2"

      ),
      tabItem(tabName = "step3"

      ),
      tabItem(tabName = "step4"

      )
    )
  )
)

server <- function(input, output, session) {
  contam <- contaminant_server("contam")

  step1a_params <- step1a_interface_server("step1a_ui", contam = contam, info=TRUE)
  step1a_output <- step1a_output_server("step1a_run", step1a_params = step1a_params)

  samples_1b <- samples_server("samples_1b", askComposite = FALSE)

  step1b_params <- step1_server("step1b_params", testIncr = TRUE)

}

shinyApp(ui, server)
