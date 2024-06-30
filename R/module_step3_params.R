step3_input <- function(id){
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(width = 12, h4("Simulation range above/below action level:"))
    ),
    fluidRow(
      column(width = 4,
             shinyWidgets::radioGroupButtons(NS(id,"input_method"),
                                             "Select data input method",
                                             choices = c(`Manual input` = "manual", `Upload a .csv` = "csv"))
      ),
      column(width = 8,
        fluidRow(
          div(
            id=NS(id, "input_manual"),
            column(width = 6,
                   textInput(NS(id,"meas.tot"), HTML("Measured total conc. (mg/kg) <br/>(separated by commas)"))
            ),
            column(width = 6,
                   textInput(NS(id,"meas.ivba"), HTML("Measured IVBA (%) <br/>(separated by commas)"))
            )
            )
          ),
        fluidRow(
          div(
            id=NS(id, "input_csv"),
            column(width = 12,
                   load_csv_ui(NS(id,"load_csv"))
            )
          )
        )
      ),
    ),
    fluidRow(
      verbatimTextOutput(NS(id,"info"))
    )
  )
}

step3_server <- function(id, info = FALSE){
  moduleServer(id, function(input, output, session){
    shinyjs::toggleElement("info", condition = info)

    csv_input <- load_csv_server("load_csv")

    out <-reactiveValues()

    observe({
      shinyjs::toggleElement("input_manual", condition = input$input_method=="manual")
      shinyjs::toggleElement("input_csv", condition = input$input_method=="csv")

      if(input$input_method=="csv"){

        out$meas.tot <- csv_input$meas.tot
        out$meas.ivba <- csv_input$meas.ivba
      }else{
        out$meas.tot <- as.numeric(strsplit(input$meas.tot, split = ",")[[1]])
        out$meas.ivba <- as.numeric(strsplit(input$meas.ivba, split = ',')[[1]])
      }
    })

    output$info <- renderPrint(reactiveValuesToList(out))
    out
  })
}
