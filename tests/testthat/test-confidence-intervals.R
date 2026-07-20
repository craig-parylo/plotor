# set-up ----------------------------------------------------------------------

# run if testing interactively
# testthat::source_test_helpers(env = globalenv())

# different confidence levels -------------------------------------------------

testthat::test_that("different confidence levels are supported", {
  lr <- get_lr_titanic()

  # summarise with 95% confidence level
  tbl_95 <- testthat::expect_no_error(
    table_or(lr, conf_level = 0.95)
  )

  # summarise with 99% confidence level
  tbl_99 <- testthat::expect_no_error(
    table_or(lr, conf_level = 0.99)
  )

  # expect the correct class of objects
  testthat::expect_s3_class(tbl_95, "tbl_df")
  testthat::expect_s3_class(tbl_99, "tbl_df")

  # expect that they produce different outputs
  testthat::expect_false(identical(tbl_95, tbl_99))
})

# fast CI estimation ----------------------------------------------------------

testthat::test_that("fast confidence intervals are supported", {
  lr <- get_lr_titanic()

  # summarise with standard confidence intervals
  tbl_std <- testthat::expect_no_error(
    table_or(lr, output = "tibble", confint_fast_estimate = FALSE)
  )

  # summarise with faster confidence intervals
  tbl_fast <- testthat::expect_no_error(
    table_or(lr, output = "tibble", confint_fast_estimate = TRUE)
  )

  # expect the correct class of objects
  testthat::expect_s3_class(tbl_std, "tbl_df")
  testthat::expect_s3_class(tbl_fast, "tbl_df")

  # expect they have similar structure
  testthat::expect_equal(object = names(tbl_std), expected = names(tbl_fast))
})
