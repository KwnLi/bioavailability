load_csv_ui <- function(id){
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(width = 6,
        fileInput(NS(id,"csv"), "Upload a csv file")
      ),
      column(width = 6,
        selectInput(NS(id,"meas.tot.col"), "Measured total concentration column",
                    choices = NULL),
        selectInput(NS(id,"meas.ivba.col"), "Measured RBA column",
                    choices = NULL)
      )
    ),
    fluidRow(
      div(id=NS(id,"info"), verbatimTextOutput(NS(id,"info")))
    )
  )
}

load_csv_server <- function(id, info = FALSE){
  moduleServer(id, function(input, output, session){
    shinyjs::toggleElement("info", condition = info)

    in_data <- reactive({
      file <- input$csv
      ext <- tools::file_ext(file$datapath)

      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))

      read.csv(file$datapath, header = TRUE)
    })

    observe({
      req(in_data())
      updateSelectInput(session = session, inputId = "meas.tot.col", choices = names(in_data()))
      updateSelectInput(session = session, inputId = "meas.ivba.col", choices = names(in_data()))
    })

    out <- reactiveValues()

    observe({
      req(input$meas.tot.col)
      out$meas.tot <- in_data()[,input$meas.tot.col]
    })

    observe({
      req(input$meas.ivba.col)
      out$meas.ivba <- in_data()[,input$meas.ivba.col]
    })

    output$info <- renderPrint(reactiveValuesToList(out))

    out
  })
}
