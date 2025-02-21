# main functions ---------------------------------------------------------------

## test successful ----
testthat::test_that("plot_or() does not produce messages or warnings", {

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

  # infertility lr model
  testthat::expect_silent({
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_infert.Rds'))
    plotor::plot_or(lr)
  })

})

testthat::test_that("table_or() does not produce messages or warnings", {

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

  # infertility lr model
  testthat::expect_silent({
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_infert.Rds'))
    plotor::table_or(lr)
  })

})

## snapshots -------
# testthat::test_that("table_or() produces output equivalent to a snapshot", {
#   testthat::skip_on_cran()
#
#   # titanic lr model
#   testthat::expect_snapshot({
#     lr <- readRDS(file = testthat::test_path('test_data', 'lr_titanic.Rds'))
#     plotor::table_or(lr)
#   },
#   cran = FALSE)
# })

testthat::test_that("plot_or() produces plots equivalent to a snapshot", {

  # titanic lr model
  vdiffr::expect_doppelganger({
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_titanic.Rds'))
    plotor::plot_or(lr)
  },
  title = "plot_titanic", cran = FALSE)

  # diabetes lr model
  vdiffr::expect_doppelganger({
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_diabetes.Rds'))
    plotor::plot_or(lr)
  }, title = "plot_diabetes", cran = FALSE)

  # infertility lr model
  vdiffr::expect_doppelganger({
    lr <- readRDS(file = testthat::test_path('test_data', 'lr_infert.Rds'))
    plotor::plot_or(lr)
  }, title = "plot_infert", cran = FALSE)
})

# testthat::test_that("table_or() produces {gt} tables equivalent to a snapshot", {
#   testthat::skip_on_cran()
#
#   # titanic lr model
#   testthat::expect_snapshot({
#     lr <- readRDS(file = testthat::test_path('test_data', 'lr_titanic.Rds'))
#     plotor::table_or(lr, output = 'gt')
#   }, cran = FALSE)
#
#   # diabetes lr model
#   testthat::expect_snapshot({
#     lr <- readRDS(file = testthat::test_path('test_data', 'lr_diabetes.Rds'))
#     plotor::table_or(lr, output = 'gt')
#   }, cran = FALSE)
#
#   # infertility lr model
#   testthat::expect_snapshot({
#     lr <- readRDS(file = testthat::test_path('test_data', 'lr_infert.Rds'))
#     plotor::table_or(lr, output = 'gt')
#   }, cran = FALSE)
#
# })

## test failure -----

testthat::test_that("table_or() and plot_or() handle issues gracefully", {

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
