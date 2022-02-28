library(shiny)
library(shinyjs)
source("app_functions.R")

#####App code#####
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Simulate error in bioavailability estimation v0.72"),
  helpText("Updated Jan. 13, 2021"),

  sidebarPanel(
    tabsetPanel(
      tabPanel(
        title = "Input",
        h4("Contaminant"),
        radioButtons("AsPb", "Select contaminant", choices = c(Pb = "Pb", As = "As"), inline=TRUE),
        numericInput("actLvl", 
                     "Enter site-specific action level (mg/kg):", 
                     400, step = 10),
        h4("Sampling Protocol"),
        radioButtons("compositeTF", "Sample aggregation", choices = c(Discrete = FALSE, Composite = TRUE)),
        em(conditionalPanel(
          condition = "input.compositeTF == 'TRUE'",
          numericInput("Xaggr", "*How many increments per composite?", 1, min = 2))),
        numericInput("tot_n", "# of samples to be analyzed for total Pb concentration", 5),
        numericInput("IVBA_n", "# of samples to be analyzed for IVBA", 3),
        radioButtons("useMeanTot", "For total metal concentration, use the:", 
                     choices = c(`Mean value` = T, `Upper 95% CI of mean` = F)),
        radioButtons("useMeanIVBA", "For IVBA, use the:", 
                     choices = c(`Mean value` = T, `Upper 95% CI of mean` = F)),
        h4("Decision Unit Assumptions"),
        HTML("Assume the true bioavailable metal concentration is +/- <b>X%</b> above or below the action level"),
        radioButtons("frcAct", "X = ", choices = c(`25%` = 0.25, `50%` = 0.5, "Custom"), inline=TRUE),
        # conditional input if fraction above/below is custom
        em(conditionalPanel(
          condition = "input.frcAct == 'Custom'",
          numericInput("frcAct_custom", "Custom value (%):", 30, step = 0.05, min = 0))),
        helpText("Note: Two scenarios will be simulated. One scenario will simulate a DU with bioavailable metal concentration at X% above the specified action level (Type 1 error is possible) and a second scenario will simulate a DU with bioavailable metal concentration at X% below the action level (Type 2 error is possible)."),
        radioButtons("CoeV_tot", "Assumed CoV for total metal concentration = ", 
                     choices = c(0.5, 1, 3, "Custom"), inline=TRUE),
        # conditional input if metal CoV is custom
        em(conditionalPanel(
          condition = "input.CoeV_tot == 'Custom'",
          numericInput("CoeV_tot_custom", "Custom metal CoV value:", 0.75, step = 0.05, min = 0))),
        radioButtons("CoeV_RBA", "Assumed CoV for RBA = ", 
                     choices = c(0.05, 0.15, 0.30, "Custom"), inline=TRUE),
        # conditional input if RBA CoV is custom
        em(conditionalPanel(
          condition = "input.CoeV_RBA == 'Custom'",
          numericInput("CoeV_RBA_custom", "Custom RBA CoV value:", 0.10, step = 0.05, min = 0))),
        radioButtons("RBAmean", "Assumed true mean soil RBA for the decision unit = ", 
                     choices = c(`60%` = 60, "Custom"), inline=TRUE),
        # conditional input if assumed mean RBA is custom
        em(conditionalPanel(
          condition = "input.RBAmean == 'Custom'",
          numericInput("RBAmean_custom", "Custom mean RBA value (%):", 50, step = 5, min = 0))),
        
        # Make multiple simulations
        h4("Site-wide Assumptions"),
        
        radioButtons("site_CoeV_tot", "Assumed CoV for total metal concentration = ", 
                     choices = c(0.5, 1, 3, "Custom"), inline=TRUE),
        # conditional input if metal CoV is custom
        em(conditionalPanel(
          condition = "input.site_CoeV_tot == 'Custom'",
          numericInput("site_CoeV_tot_custom", "Custom metal CoV value:", 0.75, step = 0.05, min = 0))),
        
        radioButtons("site_CoeV_RBA", "Assumed CoV for RBA = ", 
                     choices = c(0.05, 0.15, 0.30, "Custom"), inline=TRUE),
        # conditional input if RBA CoV is custom
        em(conditionalPanel(
          condition = "input.site_CoeV_RBA == 'Custom'",
          numericInput("site_CoeV_RBA_custom", "Custom RBA CoV value:", 0.10, step = 0.05, min = 0))),
        
        # Simulation parameters
        h4("Simulation Parameters"),
        numericInput("ncel", "Number of possible unique sampling locations in simulated decision unit", value = 1000, min = 100, step = 100),
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
        downloadButton(outputId = "downprd_BA.type2", "Predicted bioavailability"),
        br(),
        h4("Simulation R data"),
        downloadButton(outputId = "downall.type1", "Type 1 sim rds"),
        downloadButton(outputId = "downall.type2", "Type 2 sim rds")
      )
    )
  ),
  
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel("Error Results",
               h3("Type 1 error"),
               htmlOutput("Type1preamble"),
               tags$head(tags$style("#Type1preamble{
                                 font-size: 20px;
                                 }")),
               br(),
               htmlOutput("Type1text"),
               tags$head(tags$style("#Type1text{
                                 font-size: 20px;
                                 }")),
               br(),
               h3("Type 2 error"),
               htmlOutput("Type2preamble"),
               tags$head(tags$style("#Type2preamble{
                                 font-size: 20px;
                                 }")),
               br(),
               htmlOutput("Type2text"),
               tags$head(tags$style("#Type2text{
                                 font-size: 20px;
                                 }")),
               br(),
               htmlOutput("finalText")
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
  frcAct <- reactive({
    ifelse(input$frcAct=="Custom", 
           input$frcAct_custom/100, 
           as.numeric(input$frcAct))
  })
  CoeV_tot <- reactive({
    ifelse(input$CoeV_tot=="Custom", 
           input$CoeV_tot_custom, 
           as.numeric(input$CoeV_tot))
  })
  CoeV_RBA <- reactive({
    ifelse(input$CoeV_RBA=="Custom", 
           input$CoeV_RBA_custom, 
           as.numeric(input$CoeV_RBA))
  })
  RBAmean <- reactive({
    ifelse(input$RBAmean=="Custom", 
           input$RBAmean_custom, 
           as.numeric(input$RBAmean))
  })
  observe({
    AsPb.choice = input$AsPb
    comp.choice = input$compositeTF
    if(AsPb.choice == "As"){
      updateTextInput(session, "actLvl", value = 40)
      updateTextInput(session, "tot_n", label = "# of samples to be analyzed for total As concentration")
    }else if(AsPb.choice == "Pb"){
      updateTextInput(session, "actLvl", value = 400)
      updateTextInput(session, "tot_n", label = "# of samples to be analyzed for total Pb concentration")
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
          frcAct = frcAct(),
          CoeV_tot = CoeV_tot(),
          CoeV_RBA = CoeV_RBA(),
          RBAmean = RBAmean(),
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
          frcAct = -frcAct(),
          CoeV_tot = CoeV_tot(),
          CoeV_RBA = CoeV_RBA(),
          RBAmean = RBAmean(),
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
  output$Type1preamble <- renderUI({HTML("Type 1 error is defined as incorrectly concluding the bioavailable metal fraction for the decision unit is below the action level when, in reality, it is above the action level. EPA has set guidance that the probability of making a type 1 error should be < 5%.")})
  output$Type1text <- renderUI({HTML(simText(simResult()[[1]]))})
  output$Type1warn <- renderText({
    validate(
      need(simResult()[[1]]$sim_attributes$simWarnings, NULL)
    )
    paste0(simResult()[[1]]$sim_attributes$simWarnings, collapse = " ")
    })
  output$Type2plot <- renderPlot({simPlot(simResult()[[2]])})
  output$Type2preamble <- renderUI({HTML("Type 2 error is defined as incorrectly concluding the bioavailable metal fraction for the decision unit is above the action level when, in reality, it is below the action level. EPA has set guidance that the probability of making a type 2 error should be < 20%.")})
  output$Type2text <- renderUI({HTML(simText(simResult()[[2]]))})
  output$Type2warn <- renderText({
    validate(
      need(simResult()[[2]]$sim_attributes$simWarnings, NULL)
    )
    paste0(simResult()[[2]]$sim_attributes$simWarnings, collapse = " ")
  })
  output$finalText <- renderUI({HTML("Click on the <b>Sample Simulation</b> tab to see how increasing the number of samples analyzed for total Pb and IVBA improves Type 1 and 2 error probability.")})
  
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
  output$downall.type1 <- downloadHandler(
    filename = function(){"all_type1.rds"}, 
    content = function(fname){
      saveRDS(simResult()[[1]], fname)
    }
  )
  output$downall.type2 <- downloadHandler(
    filename = function(){"all_type2.rds"}, 
    content = function(fname){
      saveRDS(simResult()[[2]], fname)
    }
  )
}

shinyApp(ui = ui, server = server)
