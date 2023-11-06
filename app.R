source("app_functions_4steps.R")
source("gui_functions.R")

##### Input GUI #####
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("hr {border-top: 1px solid #707070;}"))),
  titlePanel("Simulate error in bioavailability estimation"),
  h4("Four-step version"),
  helpText(div(style = "font-weight: normal; font-style: italic", "Updated Nov. 6, 2023")),

  sidebarPanel(
    tabsetPanel(
      tabPanel(
        title = "Sample params",
        h4("Step selection"),
        selectInput("step", "Select step:", 
                    choices = c(`Step 1A: Evaluate single sampling protocol (pre-sampling)` = "5", 
                                `Step 1B: Evaluate multiple sampling protocols (pre-sampling)` = "1",
                                `Step 2: Evaluate decision tolerance of preferred sampling protocol (pre-sampling)` = "2", 
                                `Step 3: Evaluate simulation inputs (post-sampling)` = "3", 
                                `Step 4: Estimate decision accuracy and precision (post-sampling)` = "4")),
        radioButtons("AsPb", "Select contaminant", choices = c(Pb = "Pb", As = "As"), inline=TRUE),
       
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
          condition = "input.step == 1 | input.step == 2 | input.step == 5",
          numericInput("tot.n", "# of samples to be analyzed for total metal concentration", 1, min = 1)
        ),
        
        # IVBA sample input
        conditionalPanel(
          condition = "input.step == 1 | input.step == 2 | input.step == 5",
          numericInput("ivba.n", "# of samples to be analyzed for IVBA", 1, min = 1)
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
        # radioButtons("useMeanTot", "Base final bioavailable metal concentration calculation on:", 
        #              choices = c(`Mean` = T, `Upper 95% CI of mean` = F)),
        # radioButtons("useMeanIVBA", "For IVBA, use the:", 
        #              choices = c(`Mean value` = T, `Upper 95% CI of mean` = F)),
        
        hr(),
        
        ##### Step 1 GUI #####
        conditionalPanel(
          condition = "input.step == 1 | input.step == 5",
          h4("Step 1 Input"),
          conditionalPanel(
            condition = "input.step == 1",
            numericInput("sampmax", label = "Maximum number of samples to simulate:", value = 10, min = 2),
            textInput("incr.vec", "List of composite increment numbers to test, separated by commas (1=discrete sampling)")
          ),
          radioButtons("abs_frcAct", "Assumed level of soil contamination to simulate (expressed in terms of %)", 
                       choices = c(`+/-25% action level` = 0.25, 
                                   "Custom"), 
                       selected = 0.25),
          # conditional input if fraction above/below is custom
          conditionalPanel(
            condition = "input.abs_frcAct == 'Custom'",
            numericInput("frcAct_custom", 
                         label = div(style = "font-weight: normal; font-style: italic",
                                     "*Custom value (%) (enter a neg. (-) value to run a false exceedance error simulation or pos. (+) value for false compliance error simulation):"), 
                         30, step = 1, min = -Inf, max = Inf))
        ),
        
        ##### Step 2 GUI #####
        conditionalPanel(
          condition = "input.step == 2",
          h4("Step 2 Input"),
          HTML("<i>*Simulation range above/below action level:</i>"),
          numericInput("minFrcAct", label = div(style = "font-weight: normal; font-style: italic", "Minimum (%):"), value = 0, step = 10, min = 0),
          numericInput("maxFrcAct", label = div(style = "font-weight: normal; font-style: italic", "Maximum (%):"), value = 25, step = 10, min = 0),
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
          condition = "input.step < 3",
          h4("Decision error probability objective"),
          sliderInput("type1error", label = "False compliance error threshold (%)",
                      value = 5, step = 0.5,
                      min = 0, max = 100,
                      round = TRUE),
          sliderInput("type2error", label = "False exceedance error threshold (%)",
                      value = 20, step = 0.5,
                      min = 0, max = 100,
                      round = TRUE),
          
          hr(),
        ),
        
        conditionalPanel(
          condition = "input.step != 3",
          h4("Decision Unit Assumptions"),
          
          # Total distribution assumptions
          selectInput("dist_tot", "Total metal concentration data distribution:", 
                      choices = c(`log-normal` = "lognorm", normal = "normal")),
          
          # Distribution parameters for totals distribution
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
            )
          ),
          
          # RBA distribution assumptions
          selectInput("dist_rba", "RBA data distribution:",
                      choices = c(normal = "normal", uniform = "uniform", `log-normal` = "lognorm")),
          
          # Distribution parameters for RBA distribution
          conditionalPanel(
            condition = "input.step != 4",
            radioButtons("mn_rba", "RBA mean:", 
                         choices = c(`60%` = 60, "Custom"), inline=TRUE),
            # conditional input if assumed mean RBA is custom
            conditionalPanel(
              condition = "input.mn_rba == 'Custom'",
              numericInput("mn_rba_custom",
                           label = div(style = "font-weight: normal; font-style: italic", "*Custom mean RBA value (%):"),
                           50, step = 5, min = 0, max = 100)),
            radioButtons("coeV_rba", "RBA CoV:", 
                         choices = c(0.05, 0.15, 0.30, "Custom"), inline=TRUE),
            # conditional input if RBA CoV is custom
            conditionalPanel(
              condition = "input.coeV_rba == 'Custom'",
              numericInput("coeV_rba_custom", 
                           label = div(style = "font-weight: normal; font-style: italic", "*Custom RBA CoV value:"),
                           0.10, step = 0.05, min = 0))
          ),
          
          hr()
        ),
        
        actionButton(inputId = "go", label = "Run simulation")
      ),
      tabPanel(
        title = "Sim. params",
        h4("Simulation Parameters"),
        conditionalPanel(
          "input.step != 3",
          
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
          numericInput("iter", "Number of simulations", step = 1000, min = 100, value = 5000)
          )
      ),
    )
  ),
  
  mainPanel(
    tabsetPanel(
      id = "resultsTabs",
      type = "tabs",
      tabPanel("Error Results",
               h3(textOutput("titleText")),
               br(),
               tableOutput("simTable"),
               tableOutput("simIntTable"),
               htmlOutput("simIntTableText"),
               plotOutput("step1aPlot"),
               plotOutput("step1Plot"),
               plotOutput("step2Plot"),
               plotOutput("step4Plot"),
               htmlOutput("step1bText"),
               htmlOutput("step1aText"),
               htmlOutput("step2TextType1"),
               htmlOutput("step2TextType2"),
               htmlOutput("accuracyText"),
               htmlOutput("precisionText"),
               br(),
               htmlOutput("bottomNotes")
               ),
      tabPanel(
        title = "Download",
        conditionalPanel(condition = "input.step == 1", 
                         h4("Step 1 output"),
                         downloadButton(outputId = "dlStep1", "Download")
        ),
        conditionalPanel(condition = "input.step == 2", 
                         h4("Step 2 output"),
                         downloadButton(outputId = "dlStep2", "Download")
        ),
        conditionalPanel(condition = "input.step == 4", 
                         h4("Step 4 output"),
                         downloadButton(outputId = "dlStep4_accuracy", "Accuracy sim results"),
                         downloadButton(outputId = "dlStep4_precision", "Precision sim results")
        )
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
    comp.choice <- input$composite
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
    minFrcAct <- input$minFrcAct
    maxFrcAct <- input$maxFrcAct
    updateNumericInput(session, "minFrcAct", max = maxFrcAct)
    updateNumericInput(session, "maxFrcAct", min = minFrcAct)
  })
  
  # create holders for results
  simResult <- reactiveValues(
    step1a = NULL,
    step1b = NULL,
    step2 = NULL,
    step3 = NULL,
    step4 = NULL,
    stepRun = 0   # 0 = no step run yet
  )
  
  # toggle plots on/off depending on whether there are results contained in them
  observe({
    toggle(id = "step1Plot", condition = simResult$stepRun == 1)
    toggle(id = "step1bText", condition =  simResult$stepRun == 1)
    toggle(id = "step2Plot", condition = simResult$stepRun == 2)
    toggle(id = "step4Plot", condition = simResult$stepRun == 4)
    toggle(id = "simTable", condition =  simResult$stepRun > 2)
    toggle(id = "simIntTable", condition =  simResult$stepRun > 2)
    toggle(id = "simIntTableText", condition =  simResult$stepRun > 2)
    toggle(id = "step2TextType1", condition =  simResult$stepRun == 2)
    toggle(id = "step2TextType2", condition =  simResult$stepRun == 2)
    toggle(id = "step1aPlot", condition = simResult$stepRun == 5)
    toggle(id = "step1aText", condition =  simResult$stepRun == 5)
    toggle(id = "bottomNotes", condition = simResult$stepRun != 0)
  })
  
  ##### WHEN THE SIMULATE BUTTON IS PRESSED #####
  observeEvent(input$go, {
    
    # reset results when button is pressed
    simResult$step1a <- NULL
    simResult$step1b <- NULL
    simResult$step2 <- NULL
    simResult$step3 <- NULL
    simResult$step4 <- NULL
    
    # reset the step number
    simResult$stepRun <- 0
    
    if(input$step == 1){
      validate(need(input$incr.vec, "Provide sampling increments to test.")) # doesn't work as expected
      ##### Step 1B #####
      simResult$step1b <- withProgress(
        message = "Running step 1 simulations", value = 0,{
          incProgress(1/3, detail = "Simulating false compliance error")
          incr.vec <- as.numeric(strsplit(input$incr.vec, split = ",")[[1]])
          
          type1 <- step1(
            AsPb = input$AsPb,
            actLvl = input$actLvl,
            actLvlRBA = input$actLvlRBA,
            tot.n = as.numeric(input$tot.n),
            ivba.n = as.numeric(input$ivba.n),
            incr.vec = incr.vec,
            useMeanTot = TRUE,
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
          incProgress(1/3, detail = "Simulating false exceedance error")
          type2 <- step1(
            AsPb = input$AsPb,
            actLvl = input$actLvl,
            actLvlRBA = input$actLvlRBA,
            tot.n = as.numeric(input$tot.n),
            ivba.n = as.numeric(input$ivba.n),
            incr.vec = incr.vec,
            useMeanTot = TRUE,
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
      simResult$step2 <- withProgress(
        message = "Running step 2 simulations", value = 0,{
          incProgress(1/3, detail = "Simulating false compliance error")
          type1 <- step2(
            AsPb = input$AsPb,
            actLvl = input$actLvl,
            actLvlRBA = input$actLvlRBA,
            tot.n = as.numeric(input$tot.n),
            ivba.n = as.numeric(input$ivba.n),
            tot.incr = as.numeric(input$incr),
            ivba.incr = as.numeric(input$incr),
            useMeanTot = TRUE,
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
          incProgress(1/3, detail = "Simulating false exceedance error")
          type2 <- step2(
            AsPb = input$AsPb,
            actLvl = input$actLvl,
            actLvlRBA = input$actLvlRBA,
            tot.n = as.numeric(input$tot.n),
            ivba.n = as.numeric(input$ivba.n),
            tot.incr = as.numeric(input$incr),
            ivba.incr = as.numeric(input$incr),
            useMeanTot = TRUE,
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

    }else if(input$step >= 3 & input$step < 5){
      ##### Step 3 #####
      simResult$step3 <- withProgress(
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
            AsPb = input$AsPb,
            actLvl = input$actLvl,
            actLvlRBA = input$actLvlRBA,
            tot.incr = as.numeric(input$incr),
            ivba.incr = as.numeric(input$incr),
            useMeanTot = TRUE
          )
          return(step3result)
        }
      )
    }
    if(input$step == 4){
      simResult$step4 <- withProgress(
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
            AsPb = input$AsPb,
            actLvl = input$actLvl,
            actLvlRBA = input$actLvlRBA,
            tot.incr = as.numeric(input$incr),
            ivba.incr = as.numeric(input$incr),
            useMeanTot = TRUE,
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
    if(input$step == 5){
      simResult$step1a <- withProgress(
        message = "Running step 1A simulations", value = 0,{
          
          step1aresult_above <- simDU(
            AsPb = input$AsPb,
            actLvl = input$actLvl,
            actLvlRBA = input$actLvlRBA,
            tot.n = as.numeric(input$tot.n),
            ivba.n = as.numeric(input$ivba.n),
            tot.incr = as.numeric(input$incr),
            ivba.incr = as.numeric(input$incr),
            useMeanTot = TRUE,
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
            dist_tot = input$dist_tot,
            dist_rba = input$dist_rba,
            outputLvl = 2
          )
          
          incProgress(1/2, detail = "Simulating")
          # browser()
          step1aresult_below <- simDU(
            AsPb = input$AsPb,
            actLvl = input$actLvl,
            actLvlRBA = input$actLvlRBA,
            tot.n = as.numeric(input$tot.n),
            ivba.n = as.numeric(input$ivba.n),
            tot.incr = as.numeric(input$incr),
            ivba.incr = as.numeric(input$incr),
            useMeanTot = TRUE,
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
            dist_tot = input$dist_tot,
            dist_rba = input$dist_rba,
            outputLvl = 2
          )
          
          plot_s1_a <- step1a_plot(step1aresult_above)
          plot_s1_b <- step1a_plot(step1aresult_below)
          
          return(list(above = step1aresult_above, below = step1aresult_below,
                      viz_above = plot_s1_a, viz_below = plot_s1_b))
        }
      )
    }
    
    # identify step run
    simResult$stepRun <- input$step
  }
  ) # eventReactive

  
  onclick("go", runjs("window.scrollTo(0, 50)"))  # go back to top of window
  
  # outputs: display
  output$titleText <- renderText({
    titles <- data.frame(
      step.number = 1:5,
      title = c(
        "Step 1B: Evaluate multiple sampling protocols (pre-sampling)",
        "Step 2: Evaluate decision tolerance of preferred sampling protocol (pre-sampling)",
        "Step 3: Evaluate simulation inputs (post-sampling)",
        "Step 4: Estimate decision accuracy and precision (post-sampling)",
        "Step 1A: Evaluate single sampling protocol (pre-sampling)"
      )
    )
    titles[simResult$stepRun, "title"]
  })
  output$step1Plot <- renderPlot({
    if(simResult$stepRun == 1){
      s1_t1 <- step1_plot(simResult$step1b$type1, error_threshold = input$type1error)
      s1_t2 <- step1_plot(simResult$step1b$type2, error_threshold = input$type2error)
      
      plot_grid(s1_t1, s1_t2)
    }
    })
  output$step1bText <- renderUI({
    if(simResult$stepRun == 1){
      HTML(
        "** X-axis sample number listed is based on the number of samples analyzed for total metal concentration, adding one additional sample incrementally (X+1). If the input number of samples analyzed for IVBA was different than that for totals, then the number of samples listed on the x-axis would not represent IVBA sample number. For example, if the sampling protocol originally input was 5 samples analyzed for totals and 3 samples analyzed for IVBA, then the left most data point would represent 5 samples for totals and 3 samples for IVBA, and each incremental data point would represent X+1 samples analyzed for totals and IVBA respectively."
      )
    }
  })
  output$step1aPlot <- renderPlot({
    if(simResult$stepRun == 5){

      plot_grid(simResult$step1a$viz_above[[1]], simResult$step1a$viz_below[[1]])
    }
  })
  output$step1aText <- renderUI({
    if(simResult$stepRun == 5){
      HTML(paste(simResult$step1a$viz_above[[2]], 
                 simResult$step1a$viz_below[[2]], 
                 sep = "<br/>"))
    }
  })
  output$step2Plot <- renderPlot({
    if(simResult$stepRun == 2){
      s2_t1 <- step2_plot(simResult$step2$type1, error_threshold = input$type1error)
      s2_t2 <- step2_plot(simResult$step2$type2, error_threshold = input$type2error)
      
      plot_grid(s2_t1, s2_t2)
    }
    })
  output$step4Plot <- renderPlot({
    if(simResult$stepRun == 4){
      step4_plot(simResult$step4)$outplot
    }
    })
  output$simTable <- renderTable({
    if(simResult$stepRun > 2 & simResult$stepRun < 5){
      isolate(
        data.frame(`Model input` = c(
          paste0("Measured EPC (mg bioavailable ", input$AsPb, " per kg)"),
          "Measured EPC (% above/below the AL)",
          paste0("CoV in total ", input$AsPb, " across the DU "),
          "CoV in % RBA across the DU",
          "Estimated mean % RBA"),
          `Value inferred post-sampling based on sampling results` =
            c(simResult$step3$step3$meas.ba,
              100*simResult$step3$step3$meas.frcAct,
              simResult$step3$step3$coeV.tot,
              simResult$step3$step3$coeV.rba,
              simResult$step3$step3$mn.rba),
          check.names = FALSE
        )
      )
    }
  })
  output$simIntTable <- renderTable({
    if(simResult$stepRun > 2 & simResult$stepRun < 5){
      isolate(
        data.frame(`Intermediete values used to derive updated model inputs` = 
                     c(paste0("S.D. in total ", input$AsPb, " across composites**"),
                       "S.D. in % RBA across composites*"),
                   `Value inferred post-sampling based on sampling results` =
                     c(simResult$step3$step3$sd.tot,
                       simResult$step3$step3$sd.rba),
                   check.names = FALSE
        )
      )
    }
  })
  output$simIntTableText <- renderText({
    if(simResult$stepRun == 3){
      isolate(
        paste0(
          "** S.D. observed across X composites converted to CoV in total ",
          input$AsPb, 
          " or % RBA using the following equation: S.D. (sample increments )= S.D. (observed across N composites ) x  √(# increments)")
      )
    }
  })
  # output$errorText <- renderUI({HTML(simText(simResult()[[1]]))})
  output$accuracyText <- renderUI({
    if(simResult$stepRun == 4){
      HTML(step4_plot(simResult$step4)$accuracyText)
    }
    })
  output$precisionText <- renderUI({
    if(simResult$stepRun == 4){
      HTML(step4_plot(simResult$step4)$precisionText)
    }
  })
  output$step2TextType1 <- renderUI({
    if(simResult$stepRun == 2){
      HTML(step2_text_t1(simResult$step2$type1, error_threshold = input$type1error))
    }
  })
  output$step2TextType2 <- renderUI({
    if(simResult$stepRun == 2){
      HTML(step2_text_t2(simResult$step2$type2, error_threshold = input$type2error))
    }
  })
  output$bottomNotes <- renderUI({
    HTML("<b>Notes:</b>
    <br/><i>EPC</i> = Exposure Point Concentration*
    <br/><i>DU</i> = Decision Unit
    <br/><i>AL</i> = Action Level
    <br/>* Here, the EPC is defined as the bioavailability-adjusted soil metal 
    concentration (i.e., the total metal concentration (mg/kg) x % RBA), 
    which is compared to an action level that has not been adjusted for 
    bioavailability. In cases where a % RBA value other than 100% was input 
    in the Assumed RBA of the action level field when defining the action level, 
    the tool converts the RBA-adjusted action level to an action level that has 
    not been adjusted for RBA by multiplying the user input action level by the 
    user input assumed RBA of the action level (%).")
  })
  
  # outputs: download data
  output$dlStep1 <- downloadHandler(
    filename = function(){"step1_simOutput.csv"},
    content = function(fname){
      write.csv(bind_rows(simResult$step1, .id = "errorType"), fname, row.names = FALSE)
    }
  )
  output$dlStep2 <- downloadHandler(
    filename = function(){"step2_simOutput.csv"},
    content = function(fname){
      write.csv(bind_rows(simResult$step2, .id = "errorType"), fname, row.names = FALSE)
    }
  )
  output$dlStep4_accuracy <- downloadHandler(
    filename = function(){"step4_accuracySimOutput.csv"},
    content = function(fname){
      write.csv(bind_cols(simType = "accuracy", simResult$step4$accuracy.sim$DU_sim), fname, row.names = FALSE)
    }
  )
  output$dlStep4_precision <- downloadHandler(
    filename = function(){"step4_precisionSimOutput.csv"},
    content = function(fname){
      write.csv(bind_cols(simType = "precision", simResult$step4$precision.sim$DU_sim), fname, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
