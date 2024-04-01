## code to prepare `distParam` dataset

# default distribution parameters

distParam <- list(
  As = list(
    coeV.rba = c(mn = 1.118799762, lvl95 = 0.452062439),
    coeV.tot = c(mn = 0.452062439, lvl95 = 1.118799762)
  ),
  Pb = list(
    coeV.rba = c(mn = 0.55, lvl95 = 0.95),
    coeV.tot = c(mn = 1.12, lvl95 = 1.52)
  )
)

usethis::use_data(distParam, overwrite = TRUE)
