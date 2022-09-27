library(shiny)
library(shinyjs)
source("app_functions_beta.R")

#####App code#####
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("hr {border-top: 1px solid #707070;}"))),
  titlePanel("Simulate error in bioavailability estimation"),
  h4("Appendix A version 1.53"),
  helpText("This tool estimates Type 1 and 2 decision errors when incorporating RBA estimates into cleanup decisions for Pb or As contaminated soils, where Type 1 error is defined as concluding the DU does not need cleanup action when it should (false compliance), and Type 2 error is defined as concluding the DU needs cleanup action when it doesn't (false exceedance)."),
  helpText(div(style = "font-weight: normal; font-style: italic", "Updated Aug 23, 2022")),

  sidebarPanel(
    tabsetPanel(
      tabPanel(
        title = "Input",
        h4("Contaminant"),
        radioButtons("AsPb", "Select contaminant", choices = c(Pb = "Pb", As = "As"), inline=TRUE),
        numericInput("actLvl", 
                     "Enter site-specific action level (mg/kg):", 
                     400, step = 10),
        hr(),
        h4("Sampling Protocol"),
        radioButtons("compositeTF", "Sample aggregation", choices = c(Discrete = FALSE, Composite = TRUE)),
        conditionalPanel(
          condition = "input.compositeTF == 'TRUE'",
          numericInput("Xaggr", label = div(style = "font-weight: normal; font-style: italic", "*Increments per composite:"), 1, min = 2)),
        numericInput("tot_n", "# of samples to be analyzed for total metal concentration", 5),
        numericInput("IVBA_n", "# of samples to be analyzed for IVBA", 3),
        radioButtons("useMeanTot", "Base final bioavailable metal concentration calculation on:", 
                     choices = c(`Mean` = T, `Upper 95% CI of mean` = F)),
        # radioButtons("useMeanIVBA", "For IVBA, use the:", 
        #              choices = c(`Mean value` = T, `Upper 95% CI of mean` = F)),
        hr(),
        h4("Decision Unit Assumptions"),
        radioButtons("frcAct", "Assumed level of soil contamination to simulate (expressed in terms of % below/above the action level)", 
                     choices = c(`-25% (Type 2 error simulation)` = -0.25, 
                                 `+25% (Type 1 error simulation)` = 0.25, 
                                 "Custom"), 
                     selected = 0.25),
        # conditional input if fraction above/below is custom
        conditionalPanel(
          condition = "input.frcAct == 'Custom'",
          numericInput("frcAct_custom", 
                       label = div(style = "font-weight: normal; font-style: italic",
                                   "*Custom value (%) (enter a neg. (-) value to run a Type 2 error simulation or pos. (+) value for Type 1 error simulation):"), 
                       30, step = 1, min = -Inf, max = Inf)),
        selectInput("totdist", "Total metal concentration data distribution:", 
                    choices = c(`log-normal` = "lognorm", normal = "normal")),
        radioButtons("CoeV_tot", "Total metal concentration coefficient of variance (CoV):", 
                     choices = c(0.5, 1, 3, "Custom"), inline=TRUE),
        # conditional input if metal CoV is custom
        conditionalPanel(
          condition = "input.CoeV_tot == 'Custom'",
          numericInput("CoeV_tot_custom", label = div(style = "font-weight: normal; font-style: italic", "*Custom metal CoV value:"), 0.75, step = 0.05, min = 0)),
        selectInput("rbadist", "RBA data distribution:",
                    choices = c(normal = "normal", uniform = "uniform", `log-normal` = "lognorm")),
        radioButtons("CoeV_RBA", "RBA CoV:", 
                     choices = c(0.05, 0.15, 0.30, "Custom"), inline=TRUE),
        # conditional input if RBA CoV is custom
        conditionalPanel(
          condition = "input.CoeV_RBA == 'Custom'",
          numericInput("CoeV_RBA_custom", label = div(style = "font-weight: normal; font-style: italic", "*Custom RBA CoV value:"), 0.10, step = 0.05, min = 0)),
        radioButtons("RBAmean", "RBA mean:", 
                     choices = c(`60%` = 60, "Custom"), inline=TRUE),
        # conditional input if assumed mean RBA is custom
        conditionalPanel(
          condition = "input.RBAmean == 'Custom'",
          numericInput("RBAmean_custom", label = div(style = "font-weight: normal; font-style: italic", "*Custom mean RBA value (%):"), 50, step = 5, min = 0)),
        hr(),
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
        hr(),
        h4("Optional Advanced Tool-Automated Analyses"),
        selectInput("simChoice", "Select advanced analysis type", 
                    choices = c(`None` = "none", `Vary sample size` = "sample", `Vary contaminant level` = "contaminant")),
        conditionalPanel(
          condition = "input.simChoice == 'sample'",
          numericInput("sampmax", label = div(style = "font-weight: normal; font-style: italic", "*Maximum number of samples to simulate:"), value = 10, min = 2)
        ),
        conditionalPanel(
          condition = "input.simChoice == 'contaminant'",
          HTML("<i>*Simulation range above/below action level:</i>"),
          numericInput("minFrcAct", label = div(style = "font-weight: normal; font-style: italic", "Minimum (%):"), value = 10, step = 10, min = 0),
          numericInput("maxFrcAct", label = div(style = "font-weight: normal; font-style: italic", "Maximum (%):"), value = 50, step = 10, min = 0),
          numericInput("numbins", label = div(style = "font-weight: normal; font-style: italic", "Simulation intervals"), value = 10, min = 1)
        ),
        br(),
        
        actionButton(inputId = "go", label = "Run simulation")
      ),
      tabPanel("Readme",
               h4("Readme manual (.pdf format)"),
               br(),
               actionButton(inputId = 'readme',
                            label = "Download",
                            onclick = "window.open('readme.pdf', '_blank')")
      )
    )
  ),
  
  mainPanel(
    tabsetPanel(
      id = "resultsTabs",
      type = "tabs",
      tabPanel("Error Results",
               h3(textOutput("resultTitle")),
               br(),
               htmlOutput("errorText"),
               tags$head(tags$style("#errorText{
                                 font-size: 20px;
                                 }"))
               ),
      tabPanel("Precision",
               h4(textOutput("precisionTitle")),
               plotOutput("errorPrec")
               ),
      tabPanel("Tool-Automated Analysis Results",
               h4(textOutput("SampleSimTitle")),
               plotOutput("errorSimPlot"),
               textOutput("errorSimWarn")
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
    comp.choice = input$compositeTF
    if(comp.choice==FALSE){
      updateNumericInput(session, "Xaggr", value = 1)
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
    if(input$simChoice == "sample"){
      withProgress(
        message = "Running simulations", value = 0,{
          incProgress(1/2, detail = "Varying sample number")
          type1 <- simError(
            simChoice = "sample",
            AsPb = input$AsPb,
            actLvl = input$actLvl,
            tot_n = as.numeric(input$tot_n),
            IVBA_n = as.numeric(input$IVBA_n),
            compositeTF = as.logical(input$compositeTF),
            Xaggr = input$Xaggr,
            useMeanTot = as.logical(input$useMeanTot),
            useMeanIVBA = TRUE,
            frcAct = frcAct(),
            CoeV_tot = CoeV_tot(),
            CoeV_RBA = CoeV_RBA(),
            RBAmean = RBAmean(),
            error_tot = as.logical(input$error_tot),
            error_ivb = as.logical(input$error_ivb),
            error_ivb_cv = as.numeric(input$error_ivb_cv),
            ivba_model = as.logical(input$ivba_model),
            post_mean = as.logical(input$post_mean),
            iter = input$iter,
            sampmax = input$sampmax,
            dist_tot = input$totdist,
            dist_RBA = input$rbadist
          )
          list(type1)
        }
      )
    }else if(input$simChoice == "contaminant"){
      withProgress(
        message = "Running simulations", value = 0,{
          incProgress(1/2, detail = "Varying contaminant level")
          type1 <- simError(
            simChoice = "contaminant",
            AsPb = input$AsPb,
            actLvl = input$actLvl,
            tot_n = as.numeric(input$tot_n),
            IVBA_n = as.numeric(input$IVBA_n),
            compositeTF = as.logical(input$compositeTF),
            Xaggr = input$Xaggr,
            useMeanTot = as.logical(input$useMeanTot),
            useMeanIVBA = TRUE,
            frcAct = frcAct(),
            minFrcAct = ifelse(frcAct()>0,input$minFrcAct,-input$maxFrcAct)/100,
            maxFrcAct = ifelse(frcAct()>0,input$maxFrcAct,-input$minFrcAct)/100,
            CoeV_tot = CoeV_tot(),
            CoeV_RBA = CoeV_RBA(),
            RBAmean = RBAmean(),
            error_tot = as.logical(input$error_tot),
            error_ivb = as.logical(input$error_ivb),
            error_ivb_cv = as.numeric(input$error_ivb_cv),
            ivba_model = as.logical(input$ivba_model),
            post_mean = as.logical(input$post_mean),
            iter = input$iter,
            numbins = input$numbins,
            dist_tot = input$totdist,
            dist_RBA = input$rbadist
          )
          
          list(type1)
        }
      )
    }else{
      withProgress(
        message = "Running simulations", value = 0,{
          incProgress(1/2, detail = "No advanced simulations selected")
          type1 <- simError(
            simChoice = "sample",
            AsPb = input$AsPb,
            actLvl = input$actLvl,
            tot_n = as.numeric(input$tot_n),
            IVBA_n = as.numeric(input$IVBA_n),
            compositeTF = as.logical(input$compositeTF),
            Xaggr = input$Xaggr,
            useMeanTot = as.logical(input$useMeanTot),
            useMeanIVBA = TRUE,
            frcAct = frcAct(),
            CoeV_tot = CoeV_tot(),
            CoeV_RBA = CoeV_RBA(),
            RBAmean = RBAmean(),
            error_tot = as.logical(input$error_tot),
            error_ivb = as.logical(input$error_ivb),
            error_ivb_cv = as.numeric(input$error_ivb_cv),
            ivba_model = as.logical(input$ivba_model),
            post_mean = as.logical(input$post_mean),
            iter = input$iter,
            sampmax = 0,
            dist_tot = input$totdist,
            dist_RBA = input$rbadist
          )
          list(type1)
        }
      )
    }
    
  })

  
  onclick("go", runjs("window.scrollTo(0, 50)"))  # go back to top of window
  
  # outputs: display
  output$errorSimPlot <- renderPlot({
    if(input$simChoice == "sample"){
      simPlot(simResult()[[1]])
    }else if(input$simChoice == "contaminant"){
      simPlot2(simResult()[[1]])
    }
    })
  output$errorText <- renderUI({HTML(simText(simResult()[[1]]))})
  output$errorSimWarn <- renderText({
    validate(
      need(simResult()[[1]]$sim_attributes$simWarnings, NULL)
    )
    paste0(simResult()[[1]]$sim_attributes$simWarnings, collapse = " ")
    })
  output$SampleSimTitle <- renderText({
    if(input$simChoice == "none"){
      "No advanced simulations selected"
    }else{
      simTabTitle(simResult()[[1]])
    }
  })
  output$precisionTitle <- renderText({
    precisionTabTitle(simResult()[[1]])
  })
  output$resultTitle <- renderText({
    resultTabTitle(simResult()[[1]])
  })

  # output$finalText <- renderUI({
  #   if(input$simChoice == "sample"){
  #     HTML("Click on the <b>Sample Simulation</b> tab to see how increasing the number of samples analyzed for total concentration and IVBA improves Type 1 or 2 error probability.")
  #   }else if(input$simChoice == "contaminant"){
  #     HTML("Click on the <b>Sample Simulation</b> tab to see how Type 1 or 2 error probability changes with actual contamination level.")
  #   }
  #   })
    
  
  # precision output
  output$errorPrec <- renderPlot({precPlot(simResult()[[1]])})
  
  # outputs: download data
  output$downDU <- downloadHandler(
    filename = function(){"DUsamples.csv"}, 
    content = function(fname){
      write.csv(simResult()[[1]]$sim_attributes$samp.DUsample, fname, row.names = FALSE)
    }
  )
  output$downprd_BA <- downloadHandler(
    filename = function(){"prdBA.csv"}, 
    content = function(fname){
      write.csv(simResult()[[1]]$sim_attributes$samp.prd_ba, fname, row.names = FALSE)
    }
  )
  output$downall <- downloadHandler(
    filename = function(){"all.rds"}, 
    content = function(fname){
      saveRDS(simResult()[[1]], fname)
    }
  )
  output$plotData <- downloadHandler(
    filename = function(){"plotData.csv"}, 
    content = function(fname){
      write.csv(extractPlotData(simResult()[[1]])[[1]], fname, row.names = FALSE)
    }
  )
  output$plotMetadata <- downloadHandler(
    filename = function(){"plotMetadata.csv"}, 
    content = function(fname){
      write.csv(extractPlotData(simResult()[[1]])[[2]], fname, row.names = FALSE)
    }
  )
  output$downincr <- downloadHandler(
    filename = function(){"raw_incr.csv"}, 
    content = function(fname){
      write.csv(simResult()[[1]]$sim_attributes$samp.increments, fname, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
