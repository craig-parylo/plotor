# main functions ---------------------------------------------------------------
testthat::test_that("plot_or() does not produce messages or warnings", {
  testthat::expect_silent({

    # titanic lr model
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_titanic.Rds'))
    plotor::plot_or(lr)

    # diabetes lr model
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_diabetes.Rds'))
    plotor::plot_or(lr)

    # infertility lr model
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_infert.Rds'))
    plotor::plot_or(lr)

  })
})

testthat::test_that("table_or() does not produce messages or warnings", {
  testthat::expect_silent({

    # titanic lr model
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_titanic.Rds'))
    plotor::table_or(lr)

    # diabetes lr model
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_diabetes.Rds'))
    plotor::table_or(lr)

    # infertility lr model
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_infert.Rds'))
    plotor::table_or(lr)

  })
})

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
