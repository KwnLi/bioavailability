library(shiny)
source("app_functions.R")

#####App code#####
ui <- fluidPage(
  titlePanel("Simulate error in bioavailability estimation"),
  
  sidebarPanel(
    selectInput("AsPb", "Select contaminant", choices = c(Pb = "Pb", As = "As")),
    helpText("Total mass fraction and IVBA inputs"),
    textInput("tot", "Pb measurements (mg/kg)", placeholder = "nums separated by commas"),
    textInput("IVBA_meas", "IVBA measurements (%)", placeholder = "nums separated by commas"),
    br(),
    helpText("Sample options"),
    selectInput("compositeTF", "Sample aggregation", choices = c(Composite = TRUE, Discrete = FALSE)),
    conditionalPanel(
      condition = "input.compositeTF == 'TRUE'",
      numericInput("Xaggr", "If composited, how many composites per sample?", 1)),
    selectInput("useMean", "Use mean or upper 95% confidence level for estimation?", 
                choices = c(Mean = T, `Upper 95` = F)),
    br(),
    helpText("Simulation parameters"),
    numericInput("actLvl", "Site-specific soil Pb action level (mg/kg)", 400, step = 10),
    selectInput("heterogeneity", "Use default or sample variance to simulate decision unit heterogeneity?",
                choices = c(Default = "default", Sample = "sample")),
    selectInput("useHetMnTF", "Use mean or upper 95% level of heterogeneity estimate?",
                choices = c(Mean = T, `Upper 95` = F)),
    numericInput("ncel", "Number of cells in simulated decision unit", value = 1000, min = 100, step = 100),
    numericInput("iter", "Number of simulations", step = 1000, min = 100, value = 1000),
    numericInput("sampmax", "Maximum number of samples to simulate", value = 50, min = 10),
    br(),
    
    actionButton(inputId = "go", label = "Run simulation")
  ),
  
  mainPanel(
    plotOutput("resultPlot"),
    br(),
    htmlOutput("resultText"),
    br(),
    textOutput("resultWarn")
  )

)

server <- function(input, output, session){
  observe({
    AsPb.choice = input$AsPb
    comp.choice = input$compositeTF
    if(AsPb.choice == "As"){
      updateTextInput(session, "actLvl", label = "Site-specific soil As action level (mg/kg)", value = 40)
      updateTextInput(session, "tot", label = "As measurements (mg/kg)")
    }else if(AsPb.choice == "Pb"){
      updateTextInput(session, "actLvl", label = "Site-specific soil Pb action level (mg/kg)", value = 400)
      updateTextInput(session, "tot", label = "Pb measurements (mg/kg)")
    }
    if(comp.choice==FALSE){
      updateNumericInput(session, "Xaggr", value = 1)
    }
  })
  simResult <- eventReactive(input$go,{
    simError(
      AsPb = input$AsPb,
      actLvl = input$actLvl,
      tot = as.numeric(unlist(strsplit(input$tot, ","))),
      IVBA_meas = as.numeric(unlist(strsplit(input$IVBA_meas, ","))),
      compositeTF = as.logical(input$compositeTF),
      Xaggr = input$Xaggr,
      heterogeneity = input$heterogeneity,
      useHetMnTF = as.logical(input$useHetMnTF),
      useMean = as.logical(input$useMean),
      iter = input$iter,
      ncel = input$ncel,
      sampmax = input$sampmax
    )
  })
  
  output$resultPlot <- renderPlot({simPlot(simResult())})
  output$resultText <- renderUI({HTML(simText(simResult()))})
  output$resultWarn <- renderText({
    validate(
      need(simResult()$sim_attributes$simWarnings, NULL)
    )
    paste0(simResult()$sim_attributes$simWarnings, collapse = " ")
    })
}

shinyApp(ui = ui, server = server)
