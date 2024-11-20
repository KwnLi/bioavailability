download_interface <- function(id){
  tagList(
    shinyjs::useShinyjs(),  # Set up shinyjs
    downloadButton(NS(id, "downloadTables"), "Download simulation data")
  )
}

download_server <- function(id, step_output, tmpdir, stepdirname, default.downloadname="simData"){
  moduleServer(id, function(input, output, session){

    shinyjs::hide("downloadTables")  # initially disable download button

    # name of step-specific download directory
    stepdir <- tempfile(stepdirname, tmpdir = tmpdir)

    # reactive polling of download folder status
    downloadstatus <- reactivePoll(
      1000, session,
      checkFunc = function(){
        if(dir.exists(stepdir)){
          length(list.files(stepdir)) > 0  # there is something in stepdir
        }else{
          FALSE   # stepdir doesn't exist
        }
      },
      valueFunc = function(){
        if(dir.exists(stepdir)){
          length(list.files(stepdir)) > 0  # there is something in stepdir
        }else{
          FALSE   # stepdir doesn't exist
        }
      }
    )

    # turn on download button if there's something to download
    observe({
      shinyjs::toggleElement("downloadTables", condition = downloadstatus())
    })

    # enable download button if there is something in outdir
    observe({

      # if stepdir doesn't exist yet, create it. Otherwise, clear it.
      if(!dir.exists(stepdir)){
        dir.create(stepdir)   # make a temp output dir for this step and this session
      }else{
        unlink(stepdir, recursive = FALSE)   # delete contents of stepdir
      }

      step_unlist <- unlist(step_output(), recursive = FALSE)

      for(i in seq_along(step_unlist)){
        print(names(step_unlist)[i])
        if(is.data.frame(step_unlist[[i]])){
          write.csv(step_unlist[[i]], file = paste0(stepdir,"/",names(step_unlist)[i],".csv"))
        }else if(is.list(step_unlist[[i]])){
          lvl1_name <- names(step_unlist)[i]

          for(j in seq_along(step_unlist[[i]])){
            if(is.data.frame(step_unlist[[i]][[j]])){
              print(names(step_unlist[[i]])[j])
              write.csv(step_unlist[[i]][[j]],
                        file = paste0(stepdir,"/",lvl1_name,".",names(step_unlist[[i]])[j],".csv"))
            }
          }
        }
      }

      saveRDS(step_output(), paste0(stepdir,"/",stepdirname,".rds"))

    }) |>
      bindEvent(step_output())

    output$downloadTables <- downloadHandler(
      filename = paste0(default.downloadname,".zip"),
      content = function(fname){

        # zip files in the stepdir
        files2zip <- list.files(stepdir, full.names = TRUE)

        zip(fname, files2zip, flags = "-r9Xj")
      }
    )

  })
}
