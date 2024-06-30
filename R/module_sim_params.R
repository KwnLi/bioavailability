sim_params_input <- function(id){
  tagList(
    fluidRow(
      shinyjs::useShinyjs(),
      column(
        width = 4,
        numericInput(NS(id,"iter"), "Number of simulations", step = 1000, min = 100, value = 5000)
      ),
      column(
        width = 4,
        shinyWidgets::prettySwitch(NS(id,"error_tot"),
                                     "Simulate measure. error for total conc.",
                                     value = TRUE, status = "success", fill = TRUE, width = "10px"),
        div(
          id = NS(id,"input_error_tot"),
          numericInput(NS(id,"error_tot_cv"),
                       label = div(style = "font-weight: normal; font-style: italic", "*total concentration error coefficient of variation"),
                       min = 0, value = 0.050, step = 0.01)
        )
      ),
      column(
        width = 4,
        shinyWidgets::prettySwitch(NS(id,"error_ivb"),
                                     "Simulate measure. error for IVBA",
                                     value = TRUE, status = "success", fill = TRUE),
        div(
          id = NS(id,"input_error_ivb"),
          numericInput(NS(id,"error_ivb_cv"),
                       label = div(style = "font-weight: normal; font-style: italic", "*IVBA error coefficient of variation"),
                       min = 0, value = 0.050, step = 0.01)
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        shinyWidgets::materialSwitch(NS(id,"ivba_model"),
                                     "Simulate IVBA model error",
                                     value = FALSE, right = TRUE),
        div(
          id = NS(id,"input_ivba_model"),
          shinyWidgets::radioGroupButtons(NS(id,"post_mean"),
                                          label = div(style = "font-weight: normal; font-style: italic", "*Simulate IVBA model error AFTER summarizing samples?"),
                                          choiceValues = c(TRUE,FALSE), choiceNames = c("Yes","No"))
        )
      )
    ),
    fluidRow(
      div(id=NS(id,"info"), verbatimTextOutput(NS(id,"info")))
    )
  )
}

sim_params_server <- function(id, ivba_model_error=FALSE, info=FALSE){
  moduleServer(id, function(input, output, session){
    shinyjs::toggleElement("info", condition = info)

    out <- reactiveValues()

    observe({
      shinyjs::toggleElement("ivba_model", condition = ivba_model_error)
      shinyjs::toggleElement("input_error_tot", condition = input$error_tot)
      shinyjs::toggleElement("input_error_ivb", condition = input$error_ivb)
      shinyjs::toggleElement("input_ivba_model", condition = input$ivba_model)

      out$error_tot <- input$error_tot
      if(input$error_tot){
        out$error_tot_cv <- input$error_ivb_cv
      }else{
        out$error_tot_cv <- NULL
      }

      out$error_ivb <- input$error_ivb
      if(input$error_ivb){
        out$error_ivb_cv <- input$error_ivb_cv
      }else{
        out$error_ivb_cv <- NULL
      }

      out$ivba_model <- input$ivba_model

      if(input$ivba_model){
        out$post_mean <- as.logical(input$post_mean)
      }else{
        out$post_mean <- FALSE
      }

      out$iter <- input$iter
    })

    output$info <-renderPrint({reactiveValuesToList(out)})

    out
  })
}
