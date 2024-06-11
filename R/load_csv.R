load_csv_ui <- function(id){
  shiny::tagList(
    # ns <- shiny::NS(id),
    shiny::fileInput(NS(id,"csv"), "Upload a csv file"),
    checkboxInput(NS(id,"header"), "Header", TRUE),
    shiny::selectInput(NS(id,"meas.tot"), "Measured total concentration column",
                       choices = NULL),
    shiny::selectInput(NS(id,"meas.rba"), "Measured RBA column",
                       choices = NULL)
  )
}

load_csv_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    in_data <- shiny::reactive({
      file <- input$csv
      ext <- tools::file_ext(file$datapath)

      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))

      read.csv(file$datapath, header = input$header)
    })

    shiny::observe({
      shiny::req(in_data())
      shiny::updateSelectInput("meas.tot", choices = names(in_data()))
      shiny::updateSelectInput("meas.rba", choices = names(in_data()))
    })
  })
}

# ui <- fluidPage(load_csv_ui("csv"))
#
# server <- function(input, output, session){
#   load_csv_server("csv")
# }
#
# shinyApp(ui, server)
