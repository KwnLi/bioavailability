source("app_functions_4steps.R")
source("gui_functions.R")

using("shiny", "shinyjs", "cowplot", "ggplot2", "truncnorm", "dplyr", "ggpattern")

##### Input GUI #####
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
        
        # IVBA sample input
        conditionalPanel(
          condition = "input.step == 1 | input.step == 2",
          numericInput("ivba.n", "# of samples to be analyzed for IVBA", 3)
        ),
        
        # composite
        conditionalPanel(
          condition = "input.step >= 2",
          
          radioButtons("composite", "Sample aggregation", choices = c(Discrete = FALSE, Composite = TRUE), inline=TRUE),
          conditionalPanel(
            condition = "input.composite == 'TRUE'",
            numericInput("incr", label = div(style = "font-weight: normal; font-style: italic", "*Increments per composite sample:"), value=2, min = 2))
        ),
        
        # # composite IVBA
        # radioButtons("comp_ivba", "Sample aggregation (IVBA)", choices = c(Discrete = FALSE, Composite = TRUE), inline=TRUE),
        # conditionalPanel(
        #   condition = "input.comp_ivba == 'TRUE'",
        #   numericInput("ivba.incr", label = div(style = "font-weight: normal; font-style: italic", "*Increments per composite IVBA sample:"), value=2, min = 2)),

        # Mean of 95% conf. int.
        radioButtons("useMeanTot", "Base final bioavailable metal concentration calculation on:", 
                     choices = c(`Mean` = T, `Upper 95% CI of mean` = F)),
        # radioButtons("useMeanIVBA", "For IVBA, use the:", 
        #              choices = c(`Mean value` = T, `Upper 95% CI of mean` = F)),
        
        hr(),
        
        ##### Step 1 GUI #####
        conditionalPanel(
          condition = "input.step == 1",
          h4("Step 1 Input"),
          numericInput("sampmax", label = "Maximum number of samples to simulate:", value = 10, min = 2),
          textInput("incr.vec", "List of composite increment numbers to test, separated by commas (1=discrete sampling)"),
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
        
        ##### Step 2 GUI #####
        conditionalPanel(
          condition = "input.step == 2",
          h4("Step 2 Input"),
          HTML("<i>*Simulation range above/below action level:</i>"),
          numericInput("minFrcAct", label = div(style = "font-weight: normal; font-style: italic", "Minimum (%):"), value = 0, step = 10, min = 0),
          numericInput("maxFrcAct", label = div(style = "font-weight: normal; font-style: italic", "Maximum (%):"), value = 50, step = 10, min = 0),
          numericInput("numbins", label = div(style = "font-weight: normal; font-style: italic", "Simulation intervals"), value = 10, min = 1)
        ),
        
        ##### Step 3 GUI #####
        conditionalPanel(
          condition = "input.step == 3",
          h4("Step 3 Input")
        ),
        
        ##### Step 4 GUI #####
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
        
        conditionalPanel(
          condition = "input.step != 3",
          h4("Decision Unit Assumptions"),
          
          # Total distribution assumptions
          selectInput("dist_tot", "Total metal concentration data distribution:", 
                      choices = c(`log-normal` = "lognorm", normal = "normal")),
          
          # RBA distribution assumptions
          selectInput("dist_rba", "RBA data distribution:",
                      choices = c(normal = "normal", uniform = "uniform", `log-normal` = "lognorm")),
          
          # Distribution parameters for steps 1 and 2
          conditionalPanel(
            condition = "input.step != 4",
            radioButtons("coeV_tot", "Total metal concentration coefficient of variance (CoV):", 
                         choices = c(0.5, 1, 3, "Custom"), inline=TRUE),
            # conditional input if metal CoV is custom
            conditionalPanel(
              condition = "input.coeV_tot == 'Custom'",
              numericInput("coeV_tot_custom", 
                           label = div(style = "font-weight: normal; font-style: italic", "*Custom metal CoV value:"), 
                           0.75, step = 0.05, min = 0)
            ),
            radioButtons("coeV_rba", "RBA CoV:", 
                         choices = c(0.05, 0.15, 0.30, "Custom"), inline=TRUE),
            # conditional input if RBA CoV is custom
            conditionalPanel(
              condition = "input.coeV_rba == 'Custom'",
              numericInput("coeV_rba_custom", 
                           label = div(style = "font-weight: normal; font-style: italic", "*Custom RBA CoV value:"),
                           0.10, step = 0.05, min = 0)),
            radioButtons("mn_rba", "RBA mean:", 
                         choices = c(`60%` = 60, "Custom"), inline=TRUE),
            # conditional input if assumed mean RBA is custom
            conditionalPanel(
              condition = "input.mn_rba == 'Custom'",
              numericInput("mn_rba_custom",
                           label = div(style = "font-weight: normal; font-style: italic", "*Custom mean RBA value (%):"),
                           50, step = 5, min = 0, max = 100))
          ),
          
          hr()
        ),
        
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
               conditionalPanel(condition = "input.step != 3 | input.step == 4", 
                                tableOutput("simTable")
               ),
               conditionalPanel(condition = "input.step == 1 | input.step == 2",
                                plotOutput("errorSimPlot")
                                )

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

##### Server #####
server <- function(input, output, session){
  abs_frcAct <- reactive({
    ifelse(input$abs_frcAct=="Custom", 
           input$frcAct_custom/100, 
           as.numeric(input$abs_frcAct))
  })
  coeV_tot <- reactive({
    ifelse(input$coeV_tot=="Custom",
           input$coeV_tot_custom,
           as.numeric(input$coeV_tot))
  })
  coeV_rba <- reactive({
    ifelse(input$coeV_rba=="Custom",
           input$coeV_rba_custom,
           as.numeric(input$coeV_rba))
  })
  mn_rba <- reactive({
    ifelse(input$mn_rba=="Custom",
           input$mn_rba_custom,
           as.numeric(input$mn_rba))
  })
  observe({
    comp.choice = input$composite
    if(comp.choice==FALSE){
      updateNumericInput(session, "incr", value = 1)
    }else{
      updateNumericInput(session, "incr", value = 2)
    }
  })
  # observe({
  #   compivba.choice = input$comp_ivba
  #   if(compivba.choice==FALSE){
  #     updateNumericInput(session, "ivba.incr", value = 1)
  #   }else{
  #     updateNumericInput(session, "ivba.incr", value = 2)
  #   }
  # })
  observe({
    minFrcAct = input$minFrcAct
    maxFrcAct = input$maxFrcAct
    updateNumericInput(session, "minFrcAct", max = maxFrcAct)
    updateNumericInput(session, "maxFrcAct", min = minFrcAct)
  })
  
  # when simulate button is pressed
  simResult <- eventReactive(input$go, {

    if(input$step == 1){
      ##### Step 1 #####
      withProgress(
        message = "Running step 1 simulations", value = 0,{
          incProgress(1/3, detail = "Simulating type I error")
          incr.vec <- as.numeric(strsplit(input$incr.vec, split = ",")[[1]])
          
          type1 <- step1(
            AsPb = "Pb",  # restrict to Pb for manuscript
            actLvl = input$actLvl,
            actLvlRBA = input$actLvlRBA,
            tot.n = as.numeric(input$tot.n),
            ivba.n = as.numeric(input$ivba.n),
            incr.vec = incr.vec,
            useMeanTot = as.logical(input$useMeanTot),
            useMeanIVBA = TRUE,
            frcAct = abs_frcAct(),
            coeV.tot = coeV_tot(),
            coeV.rba = coeV_rba(),
            mn.rba = mn_rba(),
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
            incr.vec = incr.vec,
            useMeanTot = as.logical(input$useMeanTot),
            useMeanIVBA = TRUE,
            frcAct = -abs_frcAct(),
            coeV.tot = coeV_tot(),
            coeV.rba = coeV_rba(),
            mn.rba = mn_rba(),
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
    }else if(input$step == 2){
      ##### Step 2 #####
      withProgress(
        message = "Running step 2 simulations", value = 0,{
          incProgress(1/3, detail = "Simulating type I error")
          type1 <- step2(
            AsPb = "Pb",  # restrict to Pb for manuscript
            actLvl = input$actLvl,
            actLvlRBA = input$actLvlRBA,
            tot.n = as.numeric(input$tot.n),
            ivba.n = as.numeric(input$ivba.n),
            tot.incr = as.numeric(input$incr),
            ivba.incr = as.numeric(input$incr),
            useMeanTot = as.logical(input$useMeanTot),
            useMeanIVBA = TRUE,
            coeV.tot = coeV_tot(),
            coeV.rba = coeV_rba(),
            mn.rba = mn_rba(),
            error_tot = as.logical(input$error_tot),
            error_ivb = as.logical(input$error_ivb),
            error_ivb_cv = as.numeric(input$error_ivb_cv),
            ivba_model = as.logical(input$ivba_model),
            post_mean = as.logical(input$post_mean),
            iter = input$iter,
            minFrcAct = input$minFrcAct/100,  # minimum fraction of action level to simulate
            maxFrcAct = input$maxFrcAct/100,  # maximum fraction of action level to simulate
            numbins = input$numbins,      # nr. divisions over range of contaminant levels
            dist_tot = input$dist_tot,
            dist_rba = input$dist_rba
          )
          incProgress(1/3, detail = "Simulating type II error")
          type2 <- step2(
            AsPb = "Pb",  # restrict to Pb for manuscript
            actLvl = input$actLvl,
            actLvlRBA = input$actLvlRBA,
            tot.n = as.numeric(input$tot.n),
            ivba.n = as.numeric(input$ivba.n),
            tot.incr = as.numeric(input$incr),
            ivba.incr = as.numeric(input$incr),
            useMeanTot = as.logical(input$useMeanTot),
            useMeanIVBA = TRUE,
            coeV.tot = coeV_tot(),
            coeV.rba = coeV_rba(),
            mn.rba = mn_rba(),
            error_tot = as.logical(input$error_tot),
            error_ivb = as.logical(input$error_ivb),
            error_ivb_cv = as.numeric(input$error_ivb_cv),
            ivba_model = as.logical(input$ivba_model),
            post_mean = as.logical(input$post_mean),
            iter = input$iter,
            minFrcAct = -input$minFrcAct/100,  # minimum fraction of action level to simulate
            maxFrcAct = -input$maxFrcAct/100,  # maximum fraction of action level to simulate
            numbins = input$numbins,      # nr. divisions over range of contaminant levels            dist_tot = input$dist_tot,
            dist_rba = input$dist_rba
          )
          list(type1=type1, type2=type2)
        }
      )
    }else if(input$step == 3){
      ##### Step 3 #####
      withProgress(
        message = "Running step 3 simulations", value = 0,{
          meas.tot <- as.numeric(strsplit(input$meas.tot, split = ",")[[1]])
          meas.ivba <- as.numeric(strsplit(input$meas.ivba, split = ',')[[1]])
          validate(
            need(all(!is.na(meas.tot)), "Please only use numeric values separated by commas for total concentration input"),
            need(all(!is.na(meas.ivba)), "Please only use numeric values separated by commas for measured IVBA input")
          )
          
          incProgress(1/2, detail = "Simulating")
          step3result <- step3(
            meas.tot = meas.tot,      # actual total concentration measurements
            meas.ivba = meas.ivba,     # actual ivba measurements
            AsPb = "Pb",  # restrict to Pb for manuscript
            actLvl = input$actLvl,
            actLvlRBA = input$actLvlRBA,
            tot.incr = as.numeric(input$incr),
            ivba.incr = as.numeric(input$incr),
            useMeanTot = as.logical(input$useMeanTot)
          )
          return(list(step3 = step3result))
        }
      )
    }else if(input$step == 4){
      withProgress(
        message = "Running step 4 simulations", value = 0,{
          meas.tot <- as.numeric(strsplit(input$meas.tot, split = ",")[[1]])
          meas.ivba <- as.numeric(strsplit(input$meas.ivba, split = ',')[[1]])
          validate(
            need(all(!is.na(meas.tot)), "Please only use numeric values separated by commas for total concentration input"),
            need(all(!is.na(meas.ivba)), "Please only use numeric values separated by commas for measured IVBA input")
          )
          
          incProgress(1/2, detail = "Simulating")
          step4result <- step4(
            meas.tot = meas.tot,      # actual total concentration measurements
            meas.ivba = meas.ivba,     # actual ivba measurements
            AsPb = "Pb",  # restrict to Pb for manuscript
            actLvl = input$actLvl,
            actLvlRBA = input$actLvlRBA,
            tot.incr = as.numeric(input$incr),
            ivba.incr = as.numeric(input$incr),
            useMeanTot = as.logical(input$useMeanTot),
            error_tot = as.logical(input$error_tot),
            error_ivb = as.logical(input$error_ivb),
            error_ivb_cv = as.numeric(input$error_ivb_cv),
            ivba_model = as.logical(input$ivba_model),
            post_mean = as.logical(input$post_mean),
            iter = input$iter,
            dist_tot = input$dist_tot,
            dist_rba = input$dist_rba
          )
          return(step4result)
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
    }else if(input$step == 2){
      s2_t1 <- step2_plot(simResult()$type1)
      s2_t2 <- step2_plot(simResult()$type2)
      
      plot_grid(s2_t1, s2_t2)
    }else if(input$step == 4){
      step4_plot(simResult())
    }
    })
  output$simTable <- renderTable({
    if(input$step > 2){
      data.frame(`Model input` = c("Assumed true EPC (relative to the AL)",
                                   "CoV in total Pb across the DU ",
                                   "CoV in % RBA across the DU",
                                   "Estimated mean % RBA"),
                 `Value inferred post-sampling based on sampling results` =
                   c(simResult()$step3$meas.frcAct,
                     simResult()$step3$coeV.tot,
                     simResult()$step3$coeV.rba,
                     simResult()$step3$mn.rba),
                 check.names = FALSE
                 )
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
