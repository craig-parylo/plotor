# set up ----------------------------------------------------------------------

# run if testing interactively
# testthat::source_test_helpers(env = globalenv())

# main functions --------------------------------------------------------------

## test successful ----
testthat::test_that("`plot_or()` does not produce messages or warnings", {
  # get models that should not cause issues
  testthat::expect_silent({
    list_models <- list(
      get_lr_titanic(),
      get_lr_nhanes(),
      get_lr_ordered_factor()
    )
  })

  # iterate over some datasets that should not cause issues
  purrr::walk(
    .x = list_models,
    .f = \(.x) {
      # test that it runs without causing issue using defaults
      testthat::expect_silent(plotor::plot_or(.x))

      # test that it runs silently using a different confidence interval
      testthat::expect_silent(plotor::plot_or(.x, conf_level = 0.99))

      # test that it runs silently using fast CI estimation
      testthat::expect_silent(plotor::plot_or(
        .x,
        confint_fast_estimate = TRUE,
        assumption_checks = FALSE # needed because nhanes results in warning re: separation with fast confint method
      ))
    }
  )
})

testthat::test_that("`table_or()` does not produce messages or warnings", {
  # get models that should not cause issues
  testthat::expect_silent({
    list_models <- list(
      get_lr_titanic(),
      get_lr_nhanes(),
      get_lr_ordered_factor()
    )
  })

  # iterate over these models
  purrr::walk(
    .x = list_models,
    .f = \(.x) {
      # regular tibble output
      testthat::expect_silent({
        plotor::table_or(.x, output = "tibble")
      })

      # regular gt output
      testthat::expect_silent({
        plotor::table_or(.x, output = "gt")
      })

      # combined uni- and multivariable summary as tibble
      testthat::expect_silent({
        plotor::table_or(.x, output = "tibble", output_type = "uni_and_multi")
      })

      # combined uni- and multivariable summary as gt
      testthat::expect_silent({
        plotor::table_or(.x, output = "gt", output_type = "uni_and_multi")
      })
    }
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

## test failure -----

testthat::test_that("`table_or()` and `plot_or()` handle issues gracefully", {
  # not a binomial glm model
  testthat::expect_silent(lr <- get_nonlr_nhanes())
  testthat::expect_error(plotor::plot_or(lr))
  testthat::expect_error(plotor::table_or(lr))

  # non-valid conf_level
  testthat::expect_silent(lr <- get_lr_titanic())
  testthat::expect_error(plotor::plot_or(lr, conf_level = "95"))
  testthat::expect_error(plotor::table_or(lr, conf_level = "95"))

  # non-valid output requested
  testthat::expect_error(plotor::table_or(lr, output = "pink_elephant"))

  # non-vaild output_type requested
  testthat::expect_error(plotor::table_or(lr, output_type = "pink_elephant"))
})

## test `assumption_checks` parameter works
# setting to `FALSE` with models with known issues should not result in warnings
testthat::test_that("`assumption_checks` parameter works as expected", {
  # 1. list some models that result in at least one warning for assumptions
  testthat::expect_no_error({
    # NB, diabetes may not be an exported object from 'medicaldata'
    # list_models <- list(get_lr_diabetes(), get_lr_infert())
    list_models <- list(get_lr_infert())
  })

  # 2. iterate over these models and test
  purrr::walk(
    .x = list_models,
    .f = \(.x) {
      # test that warnings are not raised if `assumption_checks` is FALSE
      testthat::expect_silent({
        plotor::plot_or(.x, assumption_checks = FALSE)
      })
      testthat::expect_silent({
        plotor::table_or(.x, assumption_checks = FALSE)
      })
    }
  )
})

# validation functions ---------------------------------------------------------
## `conf_level` input ----
testthat::test_that("`validate_conf_level_input()` works as expected", {
  # inputs which are not single value and numeric
  purrr::walk(
    .x = list("0.95", c(0.95, 0.8), list("a")),
    .f = \(.x) testthat::expect_error(plotor:::validate_conf_level_input(.x))
  )

  # inputs within expected range
  purrr::walk(
    .x = c(0.50, 0.80, 0.95, 0.99),
    .f = \(.x) {
      testthat::expect_equal(plotor:::validate_conf_level_input(.x), .x)
    }
  )

  # inputs outside expected range - parse to valid inputs
  purrr::walk(
    .x = c(80, 95, 99, 99.9),
    .f = \(.x) {
      # expect a message
      testthat::expect_message({
        result <- plotor:::validate_conf_level_input(.x)
      })

      # expecting the return value
      testthat::expect_equal(result, .x / 100)
    }
  )

  # inputs outside expected range - expect messages informing of the change
  testthat::expect_message(plotor:::validate_conf_level_input(-1))
  testthat::expect_message(plotor:::validate_conf_level_input(100))
  testthat::expect_message(plotor:::validate_conf_level_input(95))
  testthat::expect_message(plotor:::validate_conf_level_input(99))
})

## `output` input ----
testthat::test_that("`validate_output_table_input()` works as expected", {
  # inputs within allowed type
  inputs <- c("tibble", "gt")
  purrr::walk(
    .x = inputs,
    .f = \(.x) testthat::expect_true(plotor:::validate_output_table_input(.x))
  )

  # inputs not in the expected list - expect a warning
  inputs <- c("pink_elephants", "", TRUE)
  purrr::walk(
    .x = inputs,
    .f = \(.x) testthat::expect_error(plotor:::validate_output_table_input(.x))
  )
})

## `output_type` input ----
testthat::test_that("`validate_output_table_type_input()` works as expected", {
  # inputs within the allowed range
  inputs <- c("multivariable", "uni_and_multi")
  purrr::walk(
    .x = inputs,
    .f = \(.x) {
      testthat::expect_true(
        plotor:::validate_output_table_type_input(.x)
      )
    }
  )

  # inputs outside the allowed range
  inputs <- c("pink_elephants", "", NA)
  purrr::walk(
    .x = inputs,
    .f = \(.x) {
      testthat::expect_error(
        plotor:::validate_output_table_type_input(.x)
      )
    }
  )
})

## assumption_binary_outcome() ----
testthat::test_that("`assumption_binary_outcome()` works as expected", {
  # expect no error from generating the data
  testthat::expect_silent(lr <- get_lr_triple_outcome())
  # raise error for models with more than two outcome levels
  testthat::expect_error({
    plotor::plot_or(lr)
  })
})

## assumption_no_multicollinearity() ----
testthat::test_that("`assumption_no_multicollinearity()` works as expected", {
  # successfully generate datasets with high correlation
  testthat::expect_no_error({
    list_models <- list(get_lr_correlated_two(), get_lr_correlated_four())
  })

  # these datasets should result in warnings when tested re: high correlations
  # they should also result in messages recommending the `plotor::check_or()` function
  purrr::walk(
    .x = list_models,
    .f = \(.x) {
      testthat::expect_message({
        testthat::expect_warning({
          plotor::plot_or(.x)
        })
      })
    }
  )

  # expecting to warn twice - once for multicollinearity and again for sample size
  # as well as messages re: `plotor::check_or()`
  testthat::expect_silent(lr <- get_lr_infert())
  testthat::expect_warning({
    testthat::expect_warning({
      testthat::expect_message({
        plotor::plot_or(lr)
      })
    })
  })
})

## assumption_no_separation() ----
testthat::test_that("`assumption_no_separation()` works as expected", {
  # expecting an error just from getting the data
  testthat::expect_warning({
    lr <- get_lr_separated()
  })

  # raise warning message for models with separation
  testthat::expect_warning({
    plotor:::assumption_no_separation(lr)
  })
})

## assumption_no_separation_fast() ----
testthat::test_that("`assumption_no_separation_fast()` works as expected", {
  # expecting an error just from getting the data
  testthat::expect_warning({
    lr <- get_lr_separated_large()
  })
  # raise warning message for models with separation
  testthat::expect_warning({
    plotor:::assumption_no_separation_fast(lr)
  })
})

## assumption_sample_size() ----
testthat::test_that("`assumption_sample_size()` works as expected", {
  # raise a warning message for models with too few observations

  # 1. list some models to test
  testthat::expect_silent({
    list_models <- list(
      get_lr_titanic(),
      get_lr_infert(),
      get_lr_ordered_factor(rows = 500)
    )
  })

  # 2. iterate over these models and test
  purrr::walk(
    .x = list_models,
    .f = \(.x) {
      # sample 20% of the data
      set.seed(123)
      df <-
        model.frame(.x) |>
        dplyr::slice_sample(prop = 0.2)

      # create a model from the data
      lr <-
        stats::glm(
          data = df,
          formula = formula(.x),
          family = binomial
        )

      # run the test that a warning is expected
      testthat::expect_warning({
        plotor:::assumption_sample_size(lr)
      })
    }
  )
})

## assumption_linearity() ----
testthat::test_that("`assumption_linearity()` works as expected", {
  # raise a warning message for models with non-linear relationships between a continuous predictor and the outcome

  # 1. list some models to test for non-linearity
  testthat::expect_silent({
    list_models <- list(get_lr_framingham(), get_lr_birth_weight())
  })

  # 2. iterate over these models and test
  purrr::walk(
    .x = list_models,
    .f = \(.x) {
      # run the test that a warning is expected
      testthat::expect_warning({
        plotor:::assumption_linearity(.x)
      })
    }
  )
})

# prediction functions --------------------------------------------------------
testthat::test_that("`predict_process_time()` works as expected", {
  # create a df and model to work with
  df <- get_df_large_synthetic(rows = 1e4)
  lr <- get_lr_large_synthetic(rows = 1e4)

  lr_no_fac <-
    stats::glm(
      data = df,
      family = "binomial",
      formula = outcome ~ pred4 + pred5 + pred6
    )

  # check output is an integer and non-negative ---
  # predict processing time
  result <- predict_process_time(glm = lr)
  testthat::expect_type(result, "integer")
  testthat::expect_true(result >= 0)

  # check for errors if given an invalid pred_level ---
  testthat::expect_error(
    predict_process_time(glm = lr, pred_level = 1.5)
  )
  testthat::expect_error(
    predict_process_time(glm = lr, pred_level = -0.1)
  )

  # check that consistent results are returned ---
  # that prediction time at 0.95 level is greater than at 0.90
  result_95 <- predict_process_time(glm = lr, pred_level = 0.95)
  result_90 <- predict_process_time(glm = lr, pred_level = 0.90)
  testthat::expect_gt(result_95, result_90)

  # check that handles models with no factors ---
  result <- predict_process_time(glm = lr_no_fac)
  testthat::expect_type(result, "integer")
  testthat::expect_true(result >= 0)

  # correctly handles empty glm
  testthat::expect_error(predict_process_time(NULL))
})

testthat::test_that("`double_check_confint_fast_estimate()` works as expected", {
  # create a model to work with - should load fine
  testthat::expect_silent({
    lr <- get_lr_large_synthetic(rows = 1e3)
  })

  # check that TRUE is returned when confint_fast_estimate is already TRUE ---
  testthat::expect_silent({
    result <- double_check_confint_fast_estimate(
      glm = lr,
      confint_fast_estimate = TRUE
    )
  })
  testthat::expect_true(result)

  # check returns original value when confint_fast_estimate = FALSE and runtime below inform_threshold
  result <- double_check_confint_fast_estimate(
    glm = lr,
    confint_fast_estimate = FALSE,
    inform_threshold = 15e3,
    recommend_threshold = 20e3
  )
  testthat::expect_false({
    testthat::skip_on_cran()
    result
  }) # should return original value

  # check for warning when runtime exceeds inform_threshold but below recommend_threshold
  testthat::expect_message({
    testthat::skip_on_cran()
    testthat::skip_if_not(interactive())
    double_check_confint_fast_estimate(
      glm = lr,
      confint_fast_estimate = FALSE,
      inform_threshold = 1e1,
      recommend_threshold = 1e5
    )
  })
})
