du_assum_input <- function(id){
  tagList(
    shinyjs::useShinyjs(),
    fluidRow(
      column(width = 4,
        # Total distribution assumptions
        selectInput(NS(id, "dist_tot"), "Total metal concentration data distribution:",
                    choices = c(`log-normal` = "lognorm", normal = "normal"))
      ),
      div(
        id = NS(id, "dist_tot_params"),
        column(width = 8,
          fluidRow(
            column(width = 6,
                   # Distribution parameters for totals distribution
                   radioButtons(NS(id, "coeV_tot"), "Total metal concentration coefficient of variance (CoV):",
                                choices = c(0.5, 1, 3, "Custom"), inline=TRUE)
            ),
            column(width = 6,
                   # conditional input if metal CoV is custom
                   div(
                     id = NS(id, "input_coeV_tot_custom"),
                     numericInput(NS(id, "coeV_tot_custom"),
                                  label = div(style = "font-weight: normal; font-style: italic", "*Custom metal CoV value:"),
                                  0.75, step = 0.05, min = 0)
                   )
            )
          )
        )
        )
      ),
    fluidRow(
      column(width = 4,
             # RBA distribution assumptions
             selectInput(NS(id, "dist_rba"), "RBA data distribution:",
                         choices = c(normal = "normal", uniform = "uniform", `log-normal` = "lognorm"))
      ),
      div(
        # Distribution parameters for RBA distribution
        id = NS(id,"dist_rba_params"),
        column(width = 8,
               fluidRow(
                 column(width = 6,
                        radioButtons(NS(id, "coeV_rba"), "RBA CoV:",
                                     choices = c(0.05, 0.15, 0.30, "Custom"), inline=TRUE),
                 ),
                 column(width = 6,
                        # conditional input if RBA CoV is custom
                        div(
                          id = NS(id, "input_coeV_rba_custom"),
                          numericInput(NS(id, "coeV_rba_custom"),
                                       label = div(style = "font-weight: normal; font-style: italic", "*Custom RBA CoV value:"),
                                       0.10, step = 0.05, min = 0))
                 )
               ),
               fluidRow(
                 column(width = 6,
                        radioButtons(NS(id, "mn_rba"), "RBA mean:",
                                     choices = c(`60%` = 60, "Custom"), inline=TRUE)
                 ),
                 column(width = 6,
                        # conditional input if assumed mean RBA is custom
                        div(
                          id = NS(id, "input_mn_rba_custom"),
                          numericInput(NS(id,"mn_rba_custom"),
                                       label = div(style = "font-weight: normal; font-style: italic", "*Custom mean RBA value (%):"),
                                       50, step = 5, min = 0, max = 100)
                        )
                 )
               )
        )
      )
    ),
    fluidRow(
      verbatimTextOutput(NS(id,"info"))
      )
    )
}

du_assum_server <- function(id, show.dist.params=TRUE, info=FALSE){
  moduleServer(id, function(input,output, session){
    shinyjs::toggleElement("info", condition=info)

    out <- reactiveValues()

    observe({
      out$dist_tot <- input$dist_tot
      out$dist_rba <- input$dist_rba

      if(!show.dist.params){
        shinyjs::hideElement(id="dist_tot_params")
        shinyjs::hideElement(id="dist_rba_params")

      }else{
        shinyjs::showElement(id="dist_tot_params")
        shinyjs::showElement(id="dist_rba_params")

        if(input$dist_rba=="uniform"){
          shinyjs::hideElement(id="dist_rba_params")
        }else{
          shinyjs::showElement(id="dist_rba_params")
        }

        if(input$coeV_tot=="Custom"){
          shinyjs::showElement("input_coeV_tot_custom")
          out$coeV.tot <- input$coeV_tot_custom
        }else{
          shinyjs::hideElement("input_coeV_tot_custom")
          out$coeV.tot <- as.numeric(input$coeV_tot)
        }

        if(input$mn_rba=="Custom"){
          shinyjs::showElement("input_mn_rba_custom")
          out$mn.rba <- input$mn_rba_custom
        }else{
          shinyjs::hideElement("input_mn_rba_custom")
          out$mn.rba <- as.numeric(input$mn_rba)
        }

        if(input$coeV_rba=="Custom"){
          shinyjs::showElement("input_coeV_rba_custom")
          out$coeV.rba <- input$coeV_rba_custom
        }else{
          shinyjs::hideElement("input_coeV_rba_custom")
          out$coeV.rba <- as.numeric(input$coeV_rba)
        }
      }

    })

    output$info <- renderPrint({reactiveValuesToList(out)})

    out
  })
}
