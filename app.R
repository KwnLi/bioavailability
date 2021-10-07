library(shiny)
library(shinyjs)
source("app_functions_site.R")
ucrtot <- read.csv("UCRtot.csv")
ucrivba <- read.csv("UCRivba.csv")
ucr_cov <- read.csv("UCR_CoV.csv")

#####App code#####
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("hr {border-top: 1px solid #707070;}"))),
  titlePanel("Simulate error in bioavailability site-wide estimation BETA v1.0"),
  helpText("Updated August 26, 2021"),

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
        numericInput("tot_n", "# of samples to be analyzed for total Pb concentration", 5),
        numericInput("IVBA_n", "# of samples to be analyzed for IVBA", 3),
        radioButtons("useMeanTot", "For total metal concentration, use the:", 
                     choices = c(`Mean value` = T, `Upper 95% CI of mean` = F)),
        radioButtons("useMeanIVBA", "For IVBA, use the:", 
                     choices = c(`Mean value` = T, `Upper 95% CI of mean` = F)),
        hr(),
        # h4("Decision Unit Assumptions"),
        # HTML(paste("<b>Note:</b> Two sets of simulations will be run:",
        #          "<br/>",
        #          "<b>1.)</b> Simulations assuming true bioavailable metal concentration is above the action level (Type 1 error is possible)",
        #          "<br/>",
        #          "<b>2.)</b> Simulations assuming true bioavailable metal concentration is below the action level (Type 2 error is possible)")),
        # br(),
        # br(),
        # radioButtons("frcAct", "Select percent above/below the action level to simulate", choices = c(`25%` = 0.25, `50%` = 0.5, "Custom"), inline=TRUE),
        # # conditional input if fraction above/below is custom
        # conditionalPanel(
        #   condition = "input.frcAct == 'Custom'",
        #   numericInput("frcAct_custom", label = div(style = "font-weight: normal; font-style: italic", "*Custom value (%):"), 30, step = 1, min = 0, max = 99)),
        # selectInput("totdist", "Total metal concentration data distribution:", 
        #             choices = c(`log-normal` = "lognorm", normal = "normal")),
        # radioButtons("CoeV_tot", "Total metal concentration coefficient of variance (CoV):", 
        #              choices = c(0.5, 1, 3, "Custom"), inline=TRUE),
        # # conditional input if metal CoV is custom
        # conditionalPanel(
        #   condition = "input.CoeV_tot == 'Custom'",
        #   numericInput("CoeV_tot_custom", label = div(style = "font-weight: normal; font-style: italic", "*Custom metal CoV value:"), 0.75, step = 0.05, min = 0)),
        # selectInput("rbadist", "RBA data distribution:",
        #             choices = c(normal = "normal", uniform = "uniform", `log-normal` = "lognorm")),
        # radioButtons("CoeV_RBA", "RBA CoV:", 
        #              choices = c(0.05, 0.15, 0.30, "Custom"), inline=TRUE),
        # # conditional input if RBA CoV is custom
        # conditionalPanel(
        #   condition = "input.CoeV_RBA == 'Custom'",
        #   numericInput("CoeV_RBA_custom", label = div(style = "font-weight: normal; font-style: italic", "*Custom RBA CoV value:"), 0.10, step = 0.05, min = 0)),
        # radioButtons("RBAmean", "RBA mean:", 
        #              choices = c(`60%` = 60, "Custom"), inline=TRUE),
        # # conditional input if assumed mean RBA is custom
        # conditionalPanel(
        #   condition = "input.RBAmean == 'Custom'",
        #   numericInput("RBAmean_custom", label = div(style = "font-weight: normal; font-style: italic", "*Custom mean RBA value (%):"), 50, step = 5, min = 0)),
        # hr(),
        h4("Decision Unit Simulation Parameters"),
        numericInput("ncel", "Number of possible unique sampling locations in simulated decision unit", value = 1000, min = 100, step = 100),
        numericInput("iter", "Number of simulations", step = 1000, min = 100, value = 1000),
        numericInput("sampmax", label = "Maximum number of samples to simulate", value = 50, min = 10),
        # radioButtons("simChoice", "Select simulation type", 
        #             choices = c(`Vary sample size` = "sample", `Vary contaminant level` = "contaminant")),
        # conditionalPanel(
        #   condition = "input.simChoice == 'sample'",
        #   numericInput("sampmax", label = div(style = "font-weight: normal; font-style: italic", "*Maximum number of samples to simulate:"), value = 50, min = 10)
        # ),
        # conditionalPanel(
        #   condition = "input.simChoice == 'contaminant'",
        #   HTML("<i>*Simulation range above/below action level:</i>"),
        #   numericInput("minFrcAct", label = div(style = "font-weight: normal; font-style: italic", "Minimum (%):"), value = 10, step = 10, min = 0),
        #   numericInput("maxFrcAct", label = div(style = "font-weight: normal; font-style: italic", "Maximum (%):"), value = 50, step = 10, min = 0),
        #   numericInput("numbins", label = div(style = "font-weight: normal; font-style: italic", "Simulation intervals"), value = 10, min = 1)
        # ),
        br(),
        
        actionButton(inputId = "go", label = "Run simulation")
      ),
      tabPanel(
        title = "Download",
        h4("Simulation values"),
        downloadButton(outputId = "downtot", "Measured total soil fraction"),
        downloadButton(outputId = "downivb", "Measured IVBA"),
        downloadButton(outputId = "downprd_BA", "Predicted bioavailability"),
        br(),
        h4("Simulation R data"),
        downloadButton(outputId = "downall", "sim rds"),
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
                                 font-style: italic;
                                 #font-size: 20px;
                                 }")),
               br(),
               h3("Type 2 error"),
               htmlOutput("Type2preamble"),
               tags$head(tags$style("#Type2preamble{
                                 font-style: italic;
                                 #font-size: 20px;
                                 }")),
               br(),
               htmlOutput("resultText"),
               tags$head(tags$style("#Type2text{
                                 font-size: 20px;
                                 }")),
               br(),
               htmlOutput("finalText")
               ),
      tabPanel("Sample simulation",
               h4("Error simulation results"),
               plotOutput("resultPlot"),
               textOutput("resultWarn")
               ),
      tabPanel("Precision",
               h4("Bioavailability estimate precision"),
               plotOutput("resultPrec")
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
      updateTextInput(session, "tot_n", label = "# of samples to be analyzed for total As concentration")
    }else if(AsPb.choice == "Pb"){
      updateTextInput(session, "actLvl", value = 400)
      updateTextInput(session, "tot_n", label = "# of samples to be analyzed for total Pb concentration")
    }
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
  DUparams <- eventReactive(input$go,{
    # here is where you define the means and CoV to use
    # need to define the options and make the buttons first?
    if(input$AsPb == "As"){
      tot.samples = ucrtot$totAs
      rba.samples = ucrivba$rbaAs
    }else if(input$AsPb == "Pb"){
      tot.samples = ucrtot$totPb
      rba.samples = ucrivba$rbaPb
    }
    list(tot.samples, rba.samples)
  })
  simResult <- eventReactive(input$go,{
    withProgress(
      message = "Running simulations", value = 0,{
        # replace with loop over DUs in the site
        incProgress(1/2)
        result <- simError_nonparam(
          AsPb = input$AsPb,
          actLvl = input$actLvl,
          tot_n = as.numeric(input$tot_n),
          IVBA_n = as.numeric(input$IVBA_n),
          compositeTF = as.logical(input$compositeTF),
          Xaggr = input$Xaggr,
          useMeanTot = as.logical(input$useMeanIVBA),
          useMeanIVBA = as.logical(input$useMeanTot),
          iter = input$iter,
          ncel = input$ncel,
          sampmax = input$sampmax,
          tru_mu_tot = sampleset()[[1]], 
          tru_mu_rba = sampleset()[[2]],
          CoeV_tot = sampleset()[[2]],
          CoeV_rba = sampleset()[[2]]
        )
        list(result)
      }
    )
    
  })
  
  onclick("go", runjs("window.scrollTo(0, 50)"))  # go back to top of window
  
  # outputs: display
  output$resultPlot <- renderPlot({
    simPlot(simResult()[[1]])
    })
  output$Type1preamble <- renderUI({HTML("Type 1 error is defined as incorrectly concluding the bioavailable metal fraction for the decision unit is below the action level when, in reality, it is above the action level. EPA has set guidance that the probability of making a type 1 error should be < 5%.")})
  output$resultText <- renderUI({HTML(simText(simResult()[[1]]))})
  output$resultWarn <- renderText({
    validate(
      need(simResult()[[1]]$sim_attributes$simWarnings, NULL)
    )
    paste0(simResult()[[1]]$sim_attributes$simWarnings, collapse = " ")
    })
  output$Type2preamble <- renderUI({HTML("Type 2 error is defined as incorrectly concluding the bioavailable metal fraction for the decision unit is above the action level when, in reality, it is below the action level. EPA has set guidance that the probability of making a type 2 error should be < 20%.")})
  output$finalText <- renderUI({
    HTML("Click on the <b>Sample Simulation</b> tab to see how increasing the number of samples analyzed for total concentration and IVBA improves Type 1 and 2 error probability.")
    })
    
  
  # precision output
  output$resultPrec <- renderPlot({precPlot(simResult()[[1]])})
  
  # outputs: download data
  output$downtot <- downloadHandler(
    filename = function(){"measTot.csv"}, 
    content = function(fname){
      write.csv(simResult()[[1]]$sim_attributes$samp.meas_tot, fname, row.names = FALSE)
    }
  )
  output$downivb <- downloadHandler(
    filename = function(){"measIVB.csv"}, 
    content = function(fname){
      write.csv(simResult()[[1]]$sim_attributes$samp.meas_ivb, fname, row.names = FALSE)
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
}

shinyApp(ui = ui, server = server)
