test_that("simSite works", {
  test <- simSite(DU.n = 20, mn_rba_site = 0.6, coeV_rba_site = 0.1, simDist_rba_site = "lognorm", ivba.incr = 5, error_ivb_cv = 0.1)

  # check output is a list of length 3
  testthat::expect_equal(class(test), "list")
  testthat::expect_equal(length(test), 3)
})
