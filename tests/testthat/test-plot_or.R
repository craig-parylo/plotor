# main functions ---------------------------------------------------------------

## test successful ----
testthat::test_that("`plot_or()` does not produce messages or warnings", {
  # titanic lr model
  testthat::expect_silent({
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_titanic.Rds'))
    plotor::plot_or(lr)
  })

  # diabetes lr model
  testthat::expect_silent({
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_diabetes.Rds'))
    plotor::plot_or(lr)
  })
})

testthat::test_that("`table_or()` does not produce messages or warnings", {
  # titanic lr model
  testthat::expect_silent({
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_titanic.Rds'))
    plotor::table_or(lr)
  })

  # diabetes lr model
  testthat::expect_silent({
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_diabetes.Rds'))
    plotor::table_or(lr)
  })
})

## snapshots -------

# IMPORTANT NOTE
# These tests are suspended because different versions of {ggplot2} in
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

## test failure -----

testthat::test_that("`table_or()` and `plot_or()` handle issues gracefully", {
  # not a binomial glm model
  testthat::expect_error({
    lr <- readRDS(file = testthat::test_path('test_data', 'nonlr_streptb.Rds'))
    plotor::plot_or(lr)
  })
  testthat::expect_error({
    lr <- readRDS(file = testthat::test_path('test_data', 'nonlr_streptb.Rds'))
    plotor::table_or(lr)
  })

  # non-valid conf_level
  testthat::expect_error({
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_titanic.Rds'))
    plotor::plot_or(lr, conf_level = "95")
  })
  testthat::expect_error({
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_titanic.Rds'))
    plotor::table_or(lr, conf_level = "95")
  })

  # non-valid output type requested
  testthat::expect_error({
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_titanic.Rds'))
    plotor::table_or(lr, output = "pink_elephant")
  })
})

# validation functions ---------------------------------------------------------
testthat::test_that("`validate_conf_level_input()` works as expected", {
  # inputs which are not single value and numeric
  testthat::expect_error(plotor:::validate_conf_level_input("0.95"))
  testthat::expect_error(plotor:::validate_conf_level_input(c(0.95, 0.8)))

  # inputs within expected range
  testthat::expect_equal(plotor:::validate_conf_level_input(0.50), 0.50)
  testthat::expect_equal(plotor:::validate_conf_level_input(0.80), 0.80)
  testthat::expect_equal(plotor:::validate_conf_level_input(0.95), 0.95)
  testthat::expect_equal(plotor:::validate_conf_level_input(0.99), 0.99)

  # inputs outside expected range - parse to valid inputs
  testthat::expect_equal(plotor:::validate_conf_level_input(80), 0.80)
  testthat::expect_equal(plotor:::validate_conf_level_input(95), 0.95)
  testthat::expect_equal(plotor:::validate_conf_level_input(99), 0.99)
  testthat::expect_equal(plotor:::validate_conf_level_input(99.9), 0.999)

  # inputs outside expected range - expect messages informing of the change
  testthat::expect_message(plotor:::validate_conf_level_input(-1))
  testthat::expect_message(plotor:::validate_conf_level_input(100))
  testthat::expect_message(plotor:::validate_conf_level_input(95))
  testthat::expect_message(plotor:::validate_conf_level_input(99))
})

testthat::test_that("`assumption_binary_outcome()` works as expected", {
  # raise error for models with more than two outcome levels
  testthat::expect_error({
    lr <- readRDS(
      file = testthat::test_path('test_data', 'lr_triple_outcome.Rds')
    )
    plotor::plot_or(lr)
  })
})

testthat::test_that("`assumption_no_multicollinearity()` works as expected", {
  # raise warning message for models with high correlations
  testthat::expect_warning({
    lr <- readRDS(
      file = testthat::test_path('test_data', 'lr_correlated_two.Rds')
    )
    plotor::plot_or(lr)
  })

  testthat::expect_warning({
    lr <- readRDS(
      file = testthat::test_path('test_data', 'lr_correlated_four.Rds')
    )
    plotor::plot_or(lr)
  })

  # expecting to warn twice - once for multicollinearity and again for sample size
  testthat::expect_warning({
    testthat::expect_warning({
      lr <- readRDS(file = testthat::test_path('test_data', 'lr_infert.Rds'))
      plotor::plot_or(lr)
    })
  })
})

testthat::test_that("`assumption_no_separation()` works as expected", {
  # raise warning message for models with separation
  testthat::expect_warning({
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_separated.Rds'))
    plotor:::assumption_no_separation(lr)
  })
})

testthat::test_that("`assumption_sample_size()` works as expected", {
  # raise a warning message for models with too few observations

  # 1. list some models to test
  list_models <- c(
    'lr_titanic.Rds',
    'lr_infert.Rds',
    'lr_diabetes.Rds'
  )

  # 2. iterate over these models and test
  purrr::walk(
    .x = list_models,
    .f = \(.x) {
      # load the model
      lr_old <- readRDS(file = testthat::test_path('test_data', .x))

      # sample 20% of the data
      set.seed(123)
      df <-
        model.frame(lr_old) |>
        dplyr::slice_sample(prop = 0.2)

      # create a model from the data
      lr <-
        stats::glm(
          data = df,
          formula = formula(lr_old),
          family = binomial
        )

      # run the test that a warning is expected
      testthat::expect_warning({
        plotor:::assumption_sample_size(lr)
      })
    }
  )
})
