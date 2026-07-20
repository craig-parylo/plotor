# set-up ----------------------------------------------------------------------

# run if testing interactively
# testthat::source_test_helpers(env = globalenv())

# table_or --------------------------------------------------------------------
## list some well-behaved models -----
models_good <- list(
  get_lr_titanic(),
  get_lr_nhanes(),
  get_lr_ordered_factor()
)

## returns valid objects ----
testthat::test_that("`table_or(output = 'tibble')` returns tibbles", {
  purrr::walk(
    .x = models_good,
    .f = \(.x) {
      t <- plotor::table_or(.x, output = "tibble")
      testthat::expect_s3_class(object = t, class = "tbl_df")
    }
  )
})

testthat::test_that("`table_or(output = 'gt')` returns gt objects", {
  purrr::walk(
    .x = models_good,
    .f = \(.x) {
      t <- plotor::table_or(.x, output = "gt")
      testthat::expect_s3_class(object = t, class = "gt_tbl")
    }
  )
})

## handles invalid input ----
testthat::test_that("`table_or()` handles invalid inputs", {
  # errors on a non-lr object
  testthat::expect_error(plotor::table_or(get_nonlr_nhanes()))

  # handles invalid `conf_level` input data type
  testthat::expect_error(plotor::table_or(get_lr_titanic(), conf_level = "95"))

  # handles invalid `output` input
  testthat::expect_error(plotor::table_or(
    get_lr_titanic(),
    output = "pink_elephant"
  ))

  # handles invalid `output_type` input
  testthat::expect_error(plotor::table_or(
    get_lr_titanic(),
    output = "gt",
    output_type = "pink_elephant"
  ))
})
