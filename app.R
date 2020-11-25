library(shiny)
library(shinyjs)
source("app_functions.R")

#####App code#####
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Simulate error in bioavailability estimation v0.64"),
  helpText("Updated Nov. 18, 2020"),

  sidebarPanel(
    tabsetPanel(
      tabPanel(
        title = "Input",
        radioButtons("AsPb", "Select contaminant", choices = c(Pb = "Pb", As = "As"), inline=TRUE),
        numericInput("actLvl", 
                     "Enter site-specific action level (mg/kg):", 
                     400, step = 10),
        h4("Sampling Protocol"),
        radioButtons("compositeTF", "Sample aggregation", choices = c(Discrete = FALSE, Composite = TRUE)),
        em(conditionalPanel(
          condition = "input.compositeTF == 'TRUE'",
          numericInput("Xaggr", "*How many composites per sample?", 1, min = 2))),
        numericInput("tot_n", "# of samples analyzed for total Pb concentration", 5),
        numericInput("IVBA_n", "# of samples analyzed for IVBA", 3),
        radioButtons("useMeanTot", "For total metal concentration, use the:", 
                     choices = c(`Mean value` = T, `Upper 95% CI of mean` = F)),
        radioButtons("useMeanIVBA", "For IVBA, use the:", 
                     choices = c(`Mean value` = T, `Upper 95% CI of mean` = F)),
        h4("Decision Unit Assumptions"),
        HTML("Assume the true bioavailable metal concentration is +/- <b>X%</b> above or below the action level"),
        radioButtons("frcAct", "X = ", choices = c(`25%` = 0.25, `50%` = 0.5), inline=TRUE),
        helpText("Note: Two scenarios will be simulated. One scenario will simulate a DU with bioavailable metal concentration at X% above the specified action level (Type 1 error is possible) and a second scenario will simulate a DU with bioavailable metal concentration at X% below the action level (Type 2 error is possible)."),
        radioButtons("CoeV_tot", "Assumed CoV for total metal concentration = ", 
                     choices = c(0.5, 1, 3), inline=TRUE),
        radioButtons("CoeV_RBA", "Assumed CoV for RBA = ", 
                     choices = c(0.05, 0.15, 0.30), inline=TRUE),
        radioButtons("RBAmean", "Assumed true mean soil RBA for the decision unit = ", 
                     choices = c(`60%` = 60), inline=TRUE),
        h4("Simulation Parameters"),
        numericInput("ncel", "Number of cells in simulated decision unit", value = 1000, min = 100, step = 100),
        numericInput("iter", "Number of simulations", step = 1000, min = 100, value = 1000),
        numericInput("sampmax", "Maximum number of samples to simulate", value = 50, min = 10),
        br(),
        
        actionButton(inputId = "go", label = "Run simulation")
      ),
      tabPanel(
        title = "Download",
        h4("Type 1 simulation values"),
        downloadButton(outputId = "downDU.type1", "DU samples"),
        downloadButton(outputId = "downtot.type1", "Measured total soil fraction"),
        downloadButton(outputId = "downivb.type1", "Measured IVBA"),
        downloadButton(outputId = "downprd_BA.type1", "Predicted bioavailability"),
        br(),
        h4("Type 2 simulation values"),
        downloadButton(outputId = "downDU.type2", "DU samples"),
        downloadButton(outputId = "downtot.type2", "Measured total soil fraction"),
        downloadButton(outputId = "downivb.type2", "Measured IVBA"),
        downloadButton(outputId = "downprd_BA.type2", "Predicted bioavailability")
      )
    )
  ),
  
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel("Error Results",
               h3("Type 1 error"),
               htmlOutput("Type1text"),
               tags$head(tags$style("#Type1text{
                                 font-size: 20px;
                                 }")),
               br(),
               h3("Type 2 error"),
               htmlOutput("Type2text"),
               tags$head(tags$style("#Type2text{
                                 font-size: 20px;
                                 }"))
               ),
      tabPanel("Sample simulation",
               h4("Type 1 error simulation results"),
               plotOutput("Type1plot"),
               textOutput("Type1warn"),
               br(),
               h4("Type 2 error simulation results"),
               plotOutput("Type2plot"),
               textOutput("Type2warn")
               )
    )
  )

)

server <- function(input, output, session){
  observe({
    AsPb.choice = input$AsPb
    comp.choice = input$compositeTF
    if(AsPb.choice == "As"){
      updateTextInput(session, "actLvl", value = 40)
      updateTextInput(session, "tot_n", label = "# of samples analyzed for total As concentration")
    }else if(AsPb.choice == "Pb"){
      updateTextInput(session, "actLvl", value = 400)
      updateTextInput(session, "tot_n", label = "# of samples analyzed for total Pb concentration")
    }
    if(comp.choice==FALSE){
      updateNumericInput(session, "Xaggr", value = 1)
    }
  })
  simResult <- eventReactive(input$go,{
    withProgress(
      message = "Running simulations", value = 0,{
        incProgress(1/3, detail = "Type 1 error")
        type1 <- simError(
          AsPb = input$AsPb,
          actLvl = input$actLvl,
          tot_n = as.numeric(input$tot_n),
          IVBA_n = as.numeric(input$IVBA_n),
          compositeTF = as.logical(input$compositeTF),
          Xaggr = input$Xaggr,
          useMeanTot = as.logical(input$useMeanIVBA),
          useMeanIVBA = as.logical(input$useMeanTot),
          frcAct = as.numeric(input$frcAct),
          CoeV_tot = as.numeric(input$CoeV_tot),
          CoeV_RBA = as.numeric(input$CoeV_RBA),
          RBAmean = as.numeric(input$RBAmean),
          iter = input$iter,
          ncel = input$ncel,
          sampmax = input$sampmax
        )
        
        incProgress(1/3, detail = "Type 2 error")
        
        type2 <- simError(
          AsPb = input$AsPb,
          actLvl = input$actLvl,
          tot_n = as.numeric(input$tot_n),
          IVBA_n = as.numeric(input$IVBA_n),
          compositeTF = as.logical(input$compositeTF),
          Xaggr = input$Xaggr,
          useMeanTot = as.logical(input$useMeanIVBA),
          useMeanIVBA = as.logical(input$useMeanTot),
          frcAct = -as.numeric(input$frcAct),
          CoeV_tot = as.numeric(input$CoeV_tot),
          CoeV_RBA = as.numeric(input$CoeV_RBA),
          RBAmean = as.numeric(input$RBAmean),
          iter = input$iter,
          ncel = input$ncel,
          sampmax = input$sampmax
        )
        list(type1, type2)
      }
    )
  })
  
  onclick("go", runjs("window.scrollTo(0, 50)"))  # go back to top of window
  
  # outputs: display
  output$Type1plot <- renderPlot({simPlot(simResult()[[1]])})
  output$Type1text <- renderUI({HTML(simText(simResult()[[1]]))})
  output$Type1warn <- renderText({
    validate(
      need(simResult()[[1]]$sim_attributes$simWarnings, NULL)
    )
    paste0(simResult()[[1]]$sim_attributes$simWarnings, collapse = " ")
    })
  output$Type2plot <- renderPlot({simPlot(simResult()[[2]])})
  output$Type2text <- renderUI({HTML(simText(simResult()[[2]]))})
  output$Type2warn <- renderText({
    validate(
      need(simResult()[[2]]$sim_attributes$simWarnings, NULL)
    )
    paste0(simResult()[[2]]$sim_attributes$simWarnings, collapse = " ")
  })
  
  # outputs: download data
  output$downDU.type1 <- downloadHandler(
    filename = function(){"DUsamples_type1.csv"}, 
    content = function(fname){
      write.csv(simResult()[[1]]$sim_attributes$samp.DUsample, fname, row.names = FALSE)
    }
  )
  output$downtot.type1 <- downloadHandler(
    filename = function(){"measTot_type1.csv"}, 
    content = function(fname){
      write.csv(simResult()[[1]]$sim_attributes$samp.meas_tot, fname, row.names = FALSE)
    }
  )
  output$downivb.type1 <- downloadHandler(
    filename = function(){"measIVB_type1.csv"}, 
    content = function(fname){
      write.csv(simResult()[[1]]$sim_attributes$samp.meas_ivb, fname, row.names = FALSE)
    }
  )
  output$downprd_BA.type1 <- downloadHandler(
    filename = function(){"prdBA_type1.csv"}, 
    content = function(fname){
      write.csv(simResult()[[1]]$sim_attributes$samp.prd_ba, fname, row.names = FALSE)
    }
  )
  output$downDU.type2 <- downloadHandler(
    filename = function(){"DUsamples_type2.csv"}, 
    content = function(fname){
      write.csv(simResult()[[2]]$sim_attributes$samp.DUsample, fname, row.names = FALSE)
    }
  )
  output$downtot.type2 <- downloadHandler(
    filename = function(){"measTot_type2.csv"}, 
    content = function(fname){
      write.csv(simResult()[[2]]$sim_attributes$samp.meas_tot, fname, row.names = FALSE)
    }
  )
  output$downivb.type2 <- downloadHandler(
    filename = function(){"measIVB_type2.csv"}, 
    content = function(fname){
      write.csv(simResult()[[2]]$sim_attributes$samp.meas_ivb, fname, row.names = FALSE)
    }
  )
  output$downprd_BA.type2 <- downloadHandler(
    filename = function(){"prdBA_type2.csv"}, 
    content = function(fname){
      write.csv(simResult()[[2]]$sim_attributes$samp.prd_ba, fname, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
