library(shiny)
library(shinyjs)
library(cowplot)
source("app_functions_4steps.R")
source("gui_functions.R")

#####App code#####
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("hr {border-top: 1px solid #707070;}"))),
  titlePanel("Simulate error in bioavailability estimation"),
  h4("Four step version"),
  helpText("Associated manuscript"),
  helpText(div(style = "font-weight: normal; font-style: italic", "Updated Feb 18, 2023")),

  sidebarPanel(
    tabsetPanel(
      tabPanel(
        title = "Sample params",
        h4("Step selection"),
        selectInput("step", "Select step:", 
                    choices = c(`Step 1` = "1", `Step 2` = "2", `Step 3` = "3", `Step 4` = "4")),
        # radioButtons("AsPb", "Select contaminant", choices = c(Pb = "Pb", As = "As"), inline=TRUE),
       
        hr(),
        h4("Action level"),
        
        numericInput("actLvl",
                     "Enter site-specific action level (mg/kg):",
                     400, step = 10),
        numericInput("actLvlRBA",
                     "Assumed RBA of the action level (%):",
                     60, step = 10, min = 0, max = 100),
        hr(),
        h4("Sampling Protocol"),
        
        # total concentration sample input
        conditionalPanel(
          condition = "input.step == 1 | input.step == 2",
          numericInput("tot.n", "# of samples to be analyzed for total metal concentration", 5)
        ),
        
        # composite total
        radioButtons("comp_tot", "Sample aggregation (tot. conc.)", choices = c(Discrete = FALSE, Composite = TRUE), inline=TRUE),
        conditionalPanel(
          condition = "input.comp_tot == 'TRUE'",
          numericInput("tot.incr", label = div(style = "font-weight: normal; font-style: italic", "*Increments per composite total sample:"), value=2, min = 2)),
        
        # IVBA sample input
        conditionalPanel(
          condition = "input.step == 1 | input.step == 2",
          numericInput("ivba.n", "# of samples to be analyzed for IVBA", 3)
        ),
        
        # composite IVBA
        radioButtons("comp_ivba", "Sample aggregation (IVBA)", choices = c(Discrete = FALSE, Composite = TRUE), inline=TRUE),
        conditionalPanel(
          condition = "input.comp_ivba == 'TRUE'",
          numericInput("ivba.incr", label = div(style = "font-weight: normal; font-style: italic", "*Increments per composite IVBA sample:"), value=2, min = 2)),
        
        # Mean of 95% conf. int.
        radioButtons("useMeanTot", "Base final bioavailable metal concentration calculation on:", 
                     choices = c(`Mean` = T, `Upper 95% CI of mean` = F)),
        # radioButtons("useMeanIVBA", "For IVBA, use the:", 
        #              choices = c(`Mean value` = T, `Upper 95% CI of mean` = F)),
        
        hr(),
        
        conditionalPanel(
          condition = "input.step == 1",
          h4("Step 1 Input"),
          numericInput("sampmax", label = "Maximum number of samples to simulate:", value = 10, min = 2),
          radioButtons("abs_frcAct", "Assumed level of soil contamination to simulate (expressed in terms of %)", 
                       choices = c(`+/-25% action level` = 0.25, 
                                   "Custom"), 
                       selected = 0.25),
          # conditional input if fraction above/below is custom
          conditionalPanel(
            condition = "input.abs_frcAct == 'Custom'",
            numericInput("frcAct_custom", 
                         label = div(style = "font-weight: normal; font-style: italic",
                                     "*Custom value (%) (enter a neg. (-) value to run a Type 2 error simulation or pos. (+) value for Type 1 error simulation):"), 
                         30, step = 1, min = -Inf, max = Inf))
        ),
        
        conditionalPanel(
          condition = "input.step == 2",
          h4("Step 2 Input"),
          HTML("<i>*Simulation range above/below action level:</i>"),
          numericInput("minFrcAct", label = div(style = "font-weight: normal; font-style: italic", "Minimum (%):"), value = 10, step = 10, min = 0),
          numericInput("maxFrcAct", label = div(style = "font-weight: normal; font-style: italic", "Maximum (%):"), value = 50, step = 10, min = 0),
          numericInput("numbins", label = div(style = "font-weight: normal; font-style: italic", "Simulation intervals"), value = 10, min = 1)
        ),
        
        conditionalPanel(
          condition = "input.step == 3",
          h4("Step 3 Input")
        ),
        
        conditionalPanel(
          condition = "input.step == 4",
          h4("Step 4 Input")
        ),
        
        conditionalPanel(
          condition = "input.step == 3 | input.step == 4",
          textInput("meas.tot", "Measured total conc. (mg/kg) (separated by commas)")
        ),
        conditionalPanel(
          condition = "input.step == 3 | input.step == 4",
          textInput("meas.ivba", "Measured IVBA (%) (separated by commas)")
        ),
        
        hr(),
        h4("Decision Unit Assumptions"),

        selectInput("dist_tot", "Total metal concentration data distribution:", 
                    choices = c(`log-normal` = "lognorm", normal = "normal")),
        radioButtons("coeV.tot", "Total metal concentration coefficient of variance (CoV):", 
                     choices = c(0.5, 1, 3, "Custom"), inline=TRUE),
        # conditional input if metal CoV is custom
        conditionalPanel(
          condition = "input.coeV.tot == 'Custom'",
          numericInput("coeV.tot_custom", label = div(style = "font-weight: normal; font-style: italic", "*Custom metal CoV value:"), 0.75, step = 0.05, min = 0)),
        selectInput("dist_rba", "RBA data distribution:",
                    choices = c(normal = "normal", uniform = "uniform", `log-normal` = "lognorm")),
        radioButtons("coeV.rba", "RBA CoV:", 
                     choices = c(0.05, 0.15, 0.30, "Custom"), inline=TRUE),
        # conditional input if RBA CoV is custom
        conditionalPanel(
          condition = "input.coeV.rba == 'Custom'",
          numericInput("coeV.rba_custom", label = div(style = "font-weight: normal; font-style: italic", "*Custom RBA CoV value:"), 0.10, step = 0.05, min = 0)),
        radioButtons("mn.rba", "RBA mean:", 
                     choices = c(`60%` = 60, "Custom"), inline=TRUE),
        # conditional input if assumed mean RBA is custom
        conditionalPanel(
          condition = "input.rba.mn == 'Custom'",
          numericInput("mn.rba_custom", label = div(style = "font-weight: normal; font-style: italic", "*Custom mean RBA value (%):"), 50, step = 5, min = 0)),
        hr(),
        
        actionButton(inputId = "go", label = "Run simulation")
      ),
      tabPanel(
        title = "Sim. params",
        h4("Simulation Parameters"),
        selectInput("error_tot", "Simulate measurement error for total concentration", choices = c(TRUE, FALSE)),
        selectInput("error_ivb", "Simulate measurement error for IVBA", choices = c(TRUE, FALSE)),
        conditionalPanel(
          condition = "input.error_ivb == 'TRUE'",
          numericInput("error_ivb_cv", 
                       label = div(style = "font-weight: normal; font-style: italic", "*IVBA error coefficient of variation"),
                       min = 0, value = 0.050, step = 0.01)
        ),
        selectInput("ivba_model", "Simulate IVBA model error", choices = c(TRUE, FALSE), selected = FALSE),
        conditionalPanel(
          condition = "input.ivba_model == 'TRUE'",
          selectInput("post_mean",
                      label = div(style = "font-weight: normal; font-style: italic", "*Simulate IVBA model error AFTER summarizing samples?"),
                      choices = c(TRUE, FALSE), selected = FALSE)
        ),
        numericInput("iter", "Number of simulations", step = 1000, min = 100, value = 5000),
      ),
    )
  ),
  
  mainPanel(
    tabsetPanel(
      id = "resultsTabs",
      type = "tabs",
      tabPanel("Error Results",
               # h3(textOutput("resultTitle")),
               br(),
               plotOutput("errorSimPlot")
               # tags$head(tags$style("#errorText{
               #                   font-size: 20px;
               #                   }"))
               ),
      tabPanel(
        title = "Download",
        h4("Simulation values"),
        downloadButton(outputId = "downDU", "DU samples and measured values"),
        downloadButton(outputId = "downincr", "DU raw increments"),
        downloadButton(outputId = "downprd_BA", "Predicted bioavailability"),
        downloadButton(outputId = "plotData", "Plot data"),
        downloadButton(outputId = "plotMetadata", "Metadata"),
        br(),
        h4("Simulation R data"),
        downloadButton(outputId = "downall", "Type 1 sim rds")
      )

    )
  )

)

server <- function(input, output, session){
  abs_frcAct <- reactive({
    ifelse(input$abs_frcAct=="Custom", 
           input$frcAct_custom/100, 
           as.numeric(input$abs_frcAct))
  })
  coeV.tot <- reactive({
    ifelse(input$coeV.tot=="Custom", 
           input$coeV.tot_custom, 
           as.numeric(input$coeV.tot))
  })
  coeV.rba <- reactive({
    ifelse(input$coeV.rba=="Custom", 
           input$coeV.rba_custom, 
           as.numeric(input$coeV.rba))
  })
  mn.rba <- reactive({
    ifelse(input$mn.rba=="Custom", 
           input$mn.rba_custom, 
           as.numeric(input$mn.rba))
  })
  observe({
    comptot.choice = input$comp_tot
    if(comptot.choice==FALSE){
      updateNumericInput(session, "tot.incr", value = 1)
    }else{
      updateNumericInput(session, "tot.incr", value = 2)
    }
  })
  observe({
    compivba.choice = input$comp_ivba
    if(compivba.choice==FALSE){
      updateNumericInput(session, "ivba.incr", value = 1)
    }else{
      updateNumericInput(session, "ivba.incr", value = 2)
    }
  })
  observe({
    minFrcAct = input$minFrcAct
    maxFrcAct = input$maxFrcAct
    updateNumericInput(session, "minFrcAct", max = maxFrcAct)
    updateNumericInput(session, "maxFrcAct", min = minFrcAct)
  })
  
  # when simulate button is pressed
  simResult <- eventReactive(input$go,{
    if(input$step == 1){
      withProgress(
        message = "Running simulations", value = 0,{
          incProgress(1/3, detail = "Simulating type I error")
          type1 <- step1(
            AsPb = "Pb",  # restrict to Pb for manuscript
            actLvl = input$actLvl,
            actLvlRBA = input$actLvlRBA,
            tot.n = as.numeric(input$tot.n),
            ivba.n = as.numeric(input$ivba.n),
            tot.incr = as.numeric(input$tot.incr),
            ivba.incr = as.numeric(input$ivba.incr),
            useMeanTot = as.logical(input$useMeanTot),
            useMeanIVBA = TRUE,
            frcAct = abs_frcAct(),
            coeV.tot = coeV.tot(),
            coeV.rba = coeV.rba(),
            mn.rba = mn.rba(),
            error_tot = as.logical(input$error_tot),
            error_ivb = as.logical(input$error_ivb),
            error_ivb_cv = as.numeric(input$error_ivb_cv),
            ivba_model = as.logical(input$ivba_model),
            post_mean = as.logical(input$post_mean),
            iter = input$iter,
            sampmax = input$sampmax,
            dist_tot = input$dist_tot,
            dist_rba = input$dist_rba
          )
          incProgress(1/3, detail = "Simulating type II error")
          type2 <- step1(
            AsPb = "Pb",  # restrict to Pb for manuscript
            actLvl = input$actLvl,
            actLvlRBA = input$actLvlRBA,
            tot.n = as.numeric(input$tot.n),
            ivba.n = as.numeric(input$ivba.n),
            tot.incr = as.numeric(input$tot.incr),
            ivba.incr = as.numeric(input$ivba.incr),
            useMeanTot = as.logical(input$useMeanTot),
            useMeanIVBA = TRUE,
            frcAct = -abs_frcAct(),
            coeV.tot = coeV.tot(),
            coeV.rba = coeV.rba(),
            mn.rba = mn.rba(),
            error_tot = as.logical(input$error_tot),
            error_ivb = as.logical(input$error_ivb),
            error_ivb_cv = as.numeric(input$error_ivb_cv),
            ivba_model = as.logical(input$ivba_model),
            post_mean = as.logical(input$post_mean),
            iter = input$iter,
            sampmax = input$sampmax,
            dist_tot = input$dist_tot,
            dist_rba = input$dist_rba
          )
          list(type1=type1, type2=type2)
        }
      )
    }
    
  }
  ) # eventReactive

  
  onclick("go", runjs("window.scrollTo(0, 50)"))  # go back to top of window
  
  # outputs: display
  output$errorSimPlot <- renderPlot({
    if(input$step == 1){
      s1_t1 <- step1_plot(simResult()$type1)
      s1_t2 <- step1_plot(simResult()$type2)
      
      plot_grid(s1_t1, s1_t2)
    }
    })
  # output$errorText <- renderUI({HTML(simText(simResult()[[1]]))})
  # output$errorSimWarn <- renderText({
  #   validate(
  #     need(simResult()[[1]]$sim_attributes$simWarnings, NULL)
  #   )
  #   paste0(simResult()[[1]]$sim_attributes$simWarnings, collapse = " ")
  #   })
  # output$SampleSimTitle <- renderText({
  #   if(input$simChoice == "none"){
  #     "No advanced simulations selected"
  #   }else{
  #     simTabTitle(simResult()[[1]])
  #   }
  # })
  # output$precisionTitle <- renderText({
  #   precisionTabTitle(simResult()[[1]])
  # })
  # output$resultTitle <- renderText({
  #   resultTabTitle(simResult()[[1]])
  # })

  # output$finalText <- renderUI({
  #   if(input$simChoice == "sample"){
  #     HTML("Click on the <b>Sample Simulation</b> tab to see how increasing the number of samples analyzed for total concentration and IVBA improves Type 1 or 2 error probability.")
  #   }else if(input$simChoice == "contaminant"){
  #     HTML("Click on the <b>Sample Simulation</b> tab to see how Type 1 or 2 error probability changes with actual contamination level.")
  #   }
  #   })
    
  
  # precision output
  # output$errorPrec <- renderPlot({precPlot(simResult()[[1]])})
  
  # outputs: download data
  # output$downDU <- downloadHandler(
  #   filename = function(){"DUsamples.csv"}, 
  #   content = function(fname){
  #     write.csv(simResult()[[1]]$sim_attributes$samp.DUsample, fname, row.names = FALSE)
  #   }
  # )
  # output$downprd_BA <- downloadHandler(
  #   filename = function(){"prdBA.csv"}, 
  #   content = function(fname){
  #     write.csv(simResult()[[1]]$sim_attributes$samp.prd_ba, fname, row.names = FALSE)
  #   }
  # )
  # output$downall <- downloadHandler(
  #   filename = function(){"all.rds"}, 
  #   content = function(fname){
  #     saveRDS(simResult()[[1]], fname)
  #   }
  # )
  # output$plotData <- downloadHandler(
  #   filename = function(){"plotData.csv"}, 
  #   content = function(fname){
  #     write.csv(extractPlotData(simResult()[[1]])[[1]], fname, row.names = FALSE)
  #   }
  # )
  # output$plotMetadata <- downloadHandler(
  #   filename = function(){"plotMetadata.csv"}, 
  #   content = function(fname){
  #     write.csv(extractPlotData(simResult()[[1]])[[2]], fname, row.names = FALSE)
  #   }
  # )
  # output$downincr <- downloadHandler(
  #   filename = function(){"raw_incr.csv"}, 
  #   content = function(fname){
  #     write.csv(simResult()[[1]]$sim_attributes$samp.increments, fname, row.names = FALSE)
  #   }
  # )
}

shinyApp(ui = ui, server = server)
