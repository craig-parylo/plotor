
# testthat::test_that("plotor output does not produce messages or warnings", {
#   testthat::expect_silent({
#
#         df <- datasets::Titanic |>
#           dplyr::as_tibble() |>
#           dplyr::filter(n > 0) |>
#           tidyr::uncount(weights = n) |>
#           dplyr::mutate(
#             Class = Class |>
#               forcats::fct(levels = c('1st', '2nd', '3rd', 'Crew')),
#             Sex = Sex |> forcats::fct_infreq(),
#             Age = Age |> forcats::fct_infreq(),
#             Survived = Survived |> forcats::fct(levels = c('No', 'Yes'))
#           )
#
#         lr <- stats::glm(
#           data = df,
#           family = 'binomial',
#           formula = Survived ~ Class + Sex + Age
#         )
#
#         plotor::plot_or(lr)
#
#   })
# })

# validation functions ---------------------------------------------------------
testthat::test_that("validate_conf_level_input() works as expected", {

  # inputs which are not single value and numeric
  testthat::expect_error(plotor:::validate_conf_level_input("0.95"))
  testthat::expect_error(plotor:::validate_conf_level_input(c(0.95, 0.8)))

  # inputs within expected range
  testthat::expect_equal(plotor:::validate_conf_level_input(0.50), 0.50)
  testthat::expect_equal(plotor:::validate_conf_level_input(0.80), 0.80)
  testthat::expect_equal(plotor:::validate_conf_level_input(0.95), 0.95)
  testthat::expect_equal(plotor:::validate_conf_level_input(0.99), 0.99)

  # inputs outside expected range - parse to valid inputs
  # testthat::expect_equal(plotor:::validate_conf_level_input(80), 0.80)
  # testthat::expect_equal(plotor:::validate_conf_level_input(95), 0.95)
  # testthat::expect_equal(plotor:::validate_conf_level_input(99), 0.99)
  # testthat::expect_equal(plotor:::validate_conf_level_input(99.9), 0.999)

  # inputs outside expected range - expect messages informing of the change
  testthat::expect_message(plotor:::validate_conf_level_input(-1))
  testthat::expect_message(plotor:::validate_conf_level_input(100))
  testthat::expect_message(plotor:::validate_conf_level_input(95))
  testthat::expect_message(plotor:::validate_conf_level_input(99))

})
