make_pdf_ui <- function(id){
  tagList(
    shinyjs::useShinyjs(),
    shinyWidgets::downloadBttn(
      outputId = NS(id,"makePDF"),
      label ="Make report PDF", style = "pill", color = "danger"
    )
  )
}

make_pdf_server <- function(id, template.path, temp.dir, report.params, outname = "report"){
  moduleServer(id, function(input, output, session){

    output$makePDF <- downloadHandler(
      filename = paste0(outname,".pdf"),
      content = function(file){
        # Copy the report file to a temporary directory before processing
        tempReport <- file.path(temp.dir, "report.Rmd")
        file.copy(template.path, tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        rmarkdown::render(input = tempReport,
                          output_file = file,
                          params = report.params(),
                          envir = new.env(parent = globalenv())
                          )
      }
    )
  })
}
