test_that("custom lognormal distribution works", {

  # test regular lognormal
  test_no_truncate <- custom_rlnorm(10, 5, 0.1)

  # test mean is sameish
  expect_equal(mean(test_no_truncate), 5, tolerance = 0.1)

  # test sd is sameish
  expect_equal(sd(test_no_truncate), 0.1, tolerance = 0.1)

  # test truncated lognormal
  test_truncate <- custom_rlnorm(100, 0.6, 1, truncate = TRUE, max = 1)

  # test that there are enough values in distribution
  expect_equal(length(test_truncate), 100)

  # test that truncation worked
  expect_lt(max(test_truncate), 1)
})
