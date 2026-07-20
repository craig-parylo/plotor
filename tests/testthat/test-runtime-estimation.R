# set-up ----------------------------------------------------------------------

# run if testing interactively
# testthat::source_test_helpers(env = globalenv())

# runtime esimtation ----------------------------------------------------------

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
  testthat::expect_no_warning({
    lr <- get_lr_large_synthetic(rows = 1e3)
  })

  # check that TRUE is returned when confint_fast_estimate is already TRUE ---
  testthat::expect_no_warning({
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
