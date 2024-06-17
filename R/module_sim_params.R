sim_params_input <- function(id){
  tagList(
    shinyjs::useShinyjs(),
    shinyWidgets::materialSwitch(NS(id,"error_tot"),
                                  "Simulate measurement error for total concentration",
                                  value = TRUE, right = TRUE),
    shinyWidgets::materialSwitch(NS(id,"error_ivb"),
                                  "Simulate measurement error for IVBA",
                                  value = TRUE, right = TRUE),
    div(
      id = NS(id,"input_error_ivb"),
      numericInput(NS(id,"error_ivb_cv"),
                   label = div(style = "font-weight: normal; font-style: italic", "*IVBA error coefficient of variation"),
                   min = 0, value = 0.050, step = 0.01)
    ),
    shinyWidgets::materialSwitch(NS(id,"ivba_model"),
                                  "Simulate IVBA model error",
                                  value = FALSE, right = TRUE),
    div(
      id = NS(id,"input_ivba_model"),
      shinyWidgets::radioGroupButtons(NS(id,"post_mean"),
                                    label = div(style = "font-weight: normal; font-style: italic", "*Simulate IVBA model error AFTER summarizing samples?"),
                                    choiceValues = c(TRUE,FALSE), choiceNames = c("Yes","No"))
    ),
    numericInput(NS(id,"iter"), "Number of simulations", step = 1000, min = 100, value = 5000),

    div(id=NS(id,"info"), verbatimTextOutput(NS(id,"info")))
  )
}

sim_params_server <- function(id, info=FALSE){
  moduleServer(id, function(input, output, session){
    shinyjs::toggleElement("info", condition = info)

    out <- reactiveValues()

    observe({
      shinyjs::toggleElement("input_error_ivb", condition = input$error_ivb)
      shinyjs::toggleElement("input_ivba_model", condition = input$ivba_model)

      out$error_tot <- input$error_tot
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
