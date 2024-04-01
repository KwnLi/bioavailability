## code to prepare `contam_params` dataset

# contains the regression parameters for the As and Pb IVBA functions
contam_params <- list(
  As = c(
    sepred = 19/1.96, # from 95% prediction limit for a single As RBA measurement
    m = 0.79,         # from https://doi.org/10.1080/15287394.2015.1134038
    b = 3
  ),
  Pb = c(
    sepred = 32/1.96, # from 95% prediction limit for a single Pb RBA measurement
    m = 0.878,       # from OSWER 9285.7-77 (May 2007)
    b = -2.81
  )
)

usethis::use_data(contam_params, overwrite = TRUE)
