# set-up ----------------------------------------------------------------------

# run if testing interactively
# testthat::source_test_helpers(env = globalenv())

# validations -----------------------------------------------------------------

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
