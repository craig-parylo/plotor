# set up ----------------------------------------------------------------------

# run if testing interactively
# testthat::source_test_helpers(env = globalenv())

# main functions --------------------------------------------------------------

# list some well-behaved datasets
models_good <- list(
  get_lr_titanic(),
  get_lr_nhanes(),
  get_lr_ordered_factor()
)

# plot_or ---------------------------------------------------------------------
## returns valid plot ----
testthat::test_that("`plot_or()` returns ggplot objects", {
  # test these objects produce a ggplot object
  purrr::walk(
    .x = models_good,
    .f = \(.x) {
      p <- plotor::plot_or(.x)

      testthat::expect_s3_class(object = p, class = "ggplot")
    }
  )
})

## handles invalid input ----
testthat::test_that("`plot_or()` handles invalid inputs", {
  # errors on a non-lr object
  testthat::expect_error(plotor::plot_or(get_nonlr_nhanes()))

  # handles invalid `conf_level` data type
  testthat::expect_error(plotor::plot_or(get_lr_titanic(), conf_level = "95"))
})

## suppresss assumption warnings when requested ----
testthat::test_that("`plot_or()` respects `assumption_checks = FALSE`", {
  testthat::expect_no_warning(
    object = plotor::plot_or(get_lr_infert(), assumption_checks = FALSE)
  )
})

## snapshots -------

# IMPORTANT NOTE
# These tests are suspended because different versions of {ggplot2}
# produce identical-looking plots but are somehow different internally which
# results in these tests failing if they don't use the same version of {ggplot2}
# which produced the snapshot. See issue #68 for details.
# testthat::test_that("`plot_or()` produces plots equivalent to a snapshot", {
#   # titanic lr model
#   vdiffr::expect_doppelganger(
#     {
#       lr <- readRDS(file = testthat::test_path('test_data', 'lr_titanic.Rds'))
#       plotor::plot_or(lr)
#     },
#     title = "plot_titanic",
#     cran = FALSE
#   )
#
#   # diabetes lr model
#   vdiffr::expect_doppelganger(
#     {
#       lr <- readRDS(file = testthat::test_path('test_data', 'lr_diabetes.Rds'))
#       plotor::plot_or(lr)
#     },
#     title = "plot_diabetes",
#     cran = FALSE
#   )
# })
