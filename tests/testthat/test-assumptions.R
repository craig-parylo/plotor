# set-up ----------------------------------------------------------------------

# run if testing interactively
# testthat::source_test_helpers(env = globalenv())

# assumptions -----------------------------------------------------------------
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
  testthat::expect_no_warning(lr <- get_lr_infert())
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
  testthat::expect_no_warning({
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
  testthat::expect_no_warning({
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

## assumption_extreme_values() ----
testthat::test_that("`assumption_no_extreme_values()` works as expected", {
  # raise a warning message for models with extreme / influential data points
  testthat::expect_no_warning(lr <- get_lr_influential())
  testthat::expect_warning(plotor:::assumption_no_extreme_values(glm = lr))
})
