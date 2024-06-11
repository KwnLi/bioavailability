ui <- dashboardPage(
  dashboardHeader(title="Bioavailability Sample Planning & Evaluation Tool",
                  titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      contaminant_input("contam"),
      hr(),
      menuItem("Step 1", tabName = "step1", startExpanded = TRUE,
               menuSubItem("Step 1a", tabName="step1a"),
               menuSubItem("Step 1b", tabName="step1b")),
      menuItem("Step 2", tabName = "step2"),
      menuItem("Step 3", tabName = "step3"),
      menuItem("Step 4", tabName = "step4")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "step1a",
              fluidRow(
                box(width=6,
                    step1a_samplesInput <- samples_input("samples_1a")
                    ),
                box(width=6,
                    step1a_params <- step1_input("step1a_params")
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
  samples_1a <- samples_server("samples_1a")
  samples_1b <- samples_server("samples_1b", askComposite = FALSE)

  step1a_params <- step1_server("step1a_params", testIncr = FALSE)
  step1b_params <- step1_server("step1b_params", testIncr = TRUE)
}

shinyApp(ui, server)
