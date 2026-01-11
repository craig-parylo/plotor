# -----------------------------------------------------------------------------
# BENCHMARKING PROCESSING TIME
#
# This script is used to generate data and perform benchmarking processes to
# time how long `plotor::plot_or()` takes to run using the default method for
# calculating confidence intervals.
#
# ----------------------------------------------------------------------------

#' Benchmark: Make a Tibble
#'
#' @description
#' Generates a customised tibble of data for use in logistic regression modelling.
#'
#' @param n Integer. The number of rows of data to generate.
#' @param n_fac Integer. The number of factor predictors to generate.
#' @param n_fac_levels Integer. The number of levels to generate for each factor.
#' @param n_num Integer. The number of numeric predictors to generate.
#' @param prob_yes Numeric. The probability of "Yes" levels in the outcome variable.
#'
#' @returns Tibble of data consisting of an outcome variable, "outcome", and a specified number of factor and numeric predictor variables.
#' @examples
#' bm_make_tibble(n = 1000L, n_fac = 2L, n_fac_levels = 3L, n_num = 2L, prob_yes = 0.5)
#'
#' @noRd
bm_make_tibble <- function(
  n = 1, # number of rows to generate
  n_fac = 2, # number of factor predictors
  n_fac_levels = 3, # number of factor levels
  n_num = 2, # number of numeric predictors
  prob_yes = 0.5 # probability of "Yes" in the outcome
) {
  # outcome variable
  outcome <-
    sample(
      x = c("Yes", "No"),
      size = n,
      replace = TRUE,
      prob = c(prob_yes, 1 - prob_yes)
    ) |>
    forcats::fct(levels = c("Yes", "No"))

  # start the return tibble
  df_return <- tibble::tibble(outcome = outcome)

  # predictor - factors
  if (n_fac > 0) {
    fac_df <-
      # iterate for each predictor requested
      purrr::map_dfc(
        .x = seq_len(n_fac),
        .f = ~ {
          # generate the levels for the factor
          levels_vec <- glue::glue("L{seq_len(n_fac_levels)}")

          # generate the name of the predictor
          pred_name <- glue::glue("pred_fac_{.x}")

          # create a tibble with this predictor
          tibble::tibble(
            {{ pred_name }} := sample(
              x = levels_vec,
              size = n,
              replace = TRUE
            ) |>
              forcats::fct(levels = levels_vec)
          )
        }
      )

    # add to the return object
    df_return <- dplyr::bind_cols(df_return, fac_df)
  }

  # predictor - numeric
  if (n_num > 0) {
    num_df <-
      # iterate over each predictor requested
      purrr::map_dfc(
        .x = seq_len(n_num),
        .f = ~ {
          # generate the name of the predictor
          pred_name <- glue::glue("pred_num_{.x}")

          # create a tibble with this predictor
          tibble::tibble({{ pred_name }} := rnorm(n))
        }
      )

    # add to the return object
    df_return <- dplyr::bind_cols(df_return, num_df)
  }

  # return the result
  return(df_return)
}

#' Benchmark: Create a Logistic Regression Model from a tibble
#'
#' @param df Tibble of data, as produced by `bm_make_tibble()`
#'
#' @returns Binomial logistic regression model, produced by stats::glm()
#'
#' @noRd
bm_make_model_from_tibble <- function(df) {
  # get the name of the outcome variable
  model_outcome <- names(df)[1]

  # get a list of predictor variables
  model_predictors <- subset(
    x = names(df),
    subset = !names(df) %in% c("outcome")
  ) |>
    unique()

  # create a formula
  model_formula <- paste(
    model_outcome,
    " ~ ",
    paste(model_predictors, collapse = " + ")
  ) |>
    as.formula()

  # create a model
  model <- stats::glm(
    formula = model_formula,
    family = "binomial",
    data = df
  )

  return(model)
}

#' Benchmark: time process
#'
#' @description
#' Permutations of parameters for test data are first identified before being
#' included in a logistic regression model. This model is then timed for
#' producing an output by plotor::plot_or().
#'
#' @returns Tibble containing columns for details for each model parameter and a column showing the time taken to process (in ms)
#'
#' @noRd
bm_timer <- function(workers = parallel::detectCores() - 1) {
  # set up the parallel plan
  future::plan(future::multisession, workers = workers)

  # get a tibble of parameters
  df_params <-
    tidyr::crossing(
      n_seq = seq(from = 100L, to = 10000L, by = 100L),
      n_fac_seq = seq(from = 1, to = 5L, by = 2L),
      n_fac_levels_seq = seq(from = 2, to = 5L, by = 1L),
      n_num_seq = seq(from = 1, to = 5L, by = 2L)
    )

  df_return <-
    df_params |>
    dplyr::mutate(
      mb = furrr::future_pmap(
        .options = furrr::furrr_options(seed = TRUE),
        .progress = TRUE,
        .l = list(n_seq, n_fac_seq, n_fac_levels_seq, n_num_seq),
        .f = ~ {
          # set the seed for reproducibility
          set.seed = 123

          # create the model data
          model_data <- bm_make_tibble(
            n = ..1,
            n_fac = ..2,
            n_fac_levels = ..3,
            n_num = ..4
          )
          tryCatch(
            {
              # create the model
              model <- bm_make_model_from_tibble(df = model_data)

              # test how long it takes
              result <-
                microbenchmark::microbenchmark(
                  plotor::plot_or(
                    glm_model_results = model,
                    confint_fast_estimate = FALSE,
                    assumption_checks = FALSE
                  ),
                  times = 5
                )

              # average the time taken
              time <- result$time |> median() / 1e6 # convert from microseconds to milliseconds
            },
            error = function(e) {
              # return NA if anything fails
              NA_real_
            }
          )
        }
      )
    ) |>
    # mb is a list-column of numeric values
    dplyr::mutate(time_median_ms = purrr::map_dbl(mb, identity)) |>
    dplyr::select(-mb)
}

#' Get benchmark results
#'
#' @param load_saved_data Boolean, default = TRUE. Should the results be loaded from the last save? Set to FALSE to do a fresh benchmarking process.
#' @param save_data Boolean, default = FALSE. Should the saved data be overwritten if a fresh benchmarking process completed, ignored if `load_saved_data` is set to TRUE.
#'
#' @returns Tibble of results from the benchmarking process
#'
#' @noRd
get_benchmark_results <- function(load_saved_data = TRUE, save_data = FALSE) {
  results <-
    if (load_saved_data) {
      # loading results from a stored location
      readRDS(file = here::here("data-raw", "benchmark_times.Rds"))
    } else {
      # run the benchmarking process (this could take a long while)
      cli::cli_alert(
        "Please be aware: this benchmark process can take a long time"
      )
      df_temp <- bm_timer()
      # save the data if requested
      if (save_data) {
        saveRDS(
          object = df_temp,
          file = here::here("data-raw", "benchmark_times.Rds")
        )
      }
      df_temp
    }

  # return the results
  return(results)
}

#' Assess models for performance and prediction
#'
#' @description
#' Provides measures of model performance, such as AIC and BIC, adjusted
#' R-squared and root mean squared error, that help to assess which model
#' is likely to be the better performer.
#'
#' @details
#' The measures returned by this function include:
#'
#' **AIC: Akaike Information Criterion**
#' - measures model quality while penalising complexity
#' - **Lower AIC = better model**
#' - Useful when comparing non-nested models or models with different numbers of parameters
#'
#' **BIC: Bayesian Information Criterion**
#' - similar to AIC but applies a stronger penality for extra parameters
#' - **Lower BIC = better model**
#' - Often preferred when the sample size is large
#'
#' **Adjusted R squared**
#' - indicates how much of the variance in the outcome is explained by the model, adjusted for the number of predictors
#' - Unlike plain R squared, it won't automatically increase when you add more variables
#' - **Higher adjusted R2 = better fit**, but values must be interpreted in contex (e.g., 0.3 may be good in noisy fields)
#'
#' **RMSE: Root Mean Squared Error**
#' - Measures the average size of prediction errors in the same units as the outcome variable
#' - **Lower RMSE = more precise predictions**
#' - Easy to interpret because it tells you how far off the predictions are, on average
#'
#' @param model_list Named list of models to assess
#' @param df_results Tibble of results from the benchmarking process
#'
#' @returns Tibble of summary model performance measures
#'
#' @noRd
assess_models <- function(model_list, df_results) {
  # set up
  set.seed(123)

  # get a tibble of metric performance
  df_temp_perf <- assess_model_performance(model_list = model_list)

  # get a tibble of predictive ability
  df_temp_pred <- assess_model_prediction(
    model_list = model_list,
    df_results = df_results
  )

  # join the two tables
  df_return <-
    df_temp_perf |>
    dplyr::left_join(
      y = df_temp_pred,
      by = dplyr::join_by("model")
    ) |>
    # star-rating to visualise the more appropriate models
    dplyr::mutate(
      score = scales::rescale(-abs(AIC)) +
        scales::rescale(-abs(BIC)) +
        scales::rescale(adj.r.squared) +
        scales::rescale(-mean_rmse),

      star = dplyr::case_when(
        score >= quantile(score, 0.95) ~ "***",
        score >= quantile(score, 0.6) ~ "**",
        score >= quantile(score, 0.4) ~ "*",
        .default = "."
      )
    )

  # label the df variables to guide interpretation
  label_list <- list(
    model = "Model name",
    AIC = "Lower is better",
    BIC = "Lower is better",
    adj.r.squared = "Higher is better",
    mean_rmse = "Lower is better"
  )
  labelled::var_label(df_return) <- label_list

  # return the results
  return(df_return)
}

#' Assess model performance
#'
#' @description
#' Returns summary measures, AIC, BIC and adj.R.squared, for a named list of models as produced by `broom::glance()`
#'
#' @param model_list Named list of models
#'
#' @returns Tibble of summary measures of model performance for each model
#'
#' @noRd
assess_model_performance <- function(model_list) {
  # iterate over the models and return details
  purrr::map_df(
    .x = model_list,
    .f = broom::glance,
    .id = "model"
  ) |>
    dplyr::select(model, AIC, BIC, adj.r.squared)
}

#' Assess model predictive ability
#'
#' @description
#' Performs cross-validation of each model in the supplied list of models
#'
#' @param model_list Named list of models.
#' @param df_results Tibble of benchmarking results
#'
#' @returns Tibble showing `rmse` for each model
#'
#' @noRd
assess_model_prediction <- function(model_list, df_results) {
  folds <- rsample::vfold_cv(df_results, v = 10)
  purrr::map_df(
    .x = model_list,
    .f = \(.x) assess_model_prediction_individual(model = .x, folds = folds),
    .id = "model"
  )
}

#' Assess model predictive ability for an individual model
#'
#' @description
#' Performs cross-validation for a single supplied model
#'
#' @param model Linear regression model produced using `stats::lm()`
#' @param folds 10-fold cross validation
#'
#' @returns Tibble containing the model name and the rmse estimate
#'
#' @noRd
assess_model_prediction_individual <- function(model, folds) {
  purrr::map_dfr(
    .x = folds$splits,
    .f = function(split) {
      train <- rsample::analysis(split)
      test <- rsample::assessment(split)
      fit <- update(model, data = train)
      tibble::tibble(
        rmse = yardstick::rmse_vec(
          truth = log(test$time_median_ms),
          estimate = predict(fit, newdata = test)
        )
      )
    }
  ) |>
    dplyr::summarise(mean_rmse = mean(rmse))
}

collate_model_candidates <- function(results) {
  # fit some candidate models
  # additive model
  m1 <-
    stats::lm(
      formula = log(time_median_ms) ~ n_seq +
        n_fac_seq +
        n_fac_levels_seq +
        n_num_seq,
      data = results
    )

  # interaction model
  m2 <-
    stats::lm(
      formula = log(time_median_ms) ~ n_seq +
        n_num_seq +
        n_fac_seq * n_fac_levels_seq +
        n_seq:n_num_seq,
      data = results
    )

  # full
  m3 <-
    stats::lm(
      formula = log(time_median_ms) ~ n_seq *
        (n_fac_seq * n_fac_levels_seq) *
        n_num_seq,
      data = results
    )

  # log n
  m4 <-
    stats::lm(
      formula = log(time_median_ms) ~ log(n_seq) +
        n_fac_seq +
        n_fac_levels_seq +
        n_num_seq,
      data = results
    )

  # spline
  m5 <-
    stats::lm(
      formula = log(time_median_ms) ~ splines::bs(n_seq, df = 3) +
        n_fac_seq +
        n_fac_levels_seq +
        n_num_seq,
      data = results
    )

  # m4 but a bit more complex
  m6 <-
    stats::lm(
      formula = log(time_median_ms) ~ log(n_seq) +
        n_fac_seq * n_fac_levels_seq +
        n_num_seq,
      data = results
    )

  # m6 but a bit more complex
  m7 <-
    stats::lm(
      formula = log(time_median_ms) ~ log(n_seq) +
        n_fac_seq * n_fac_levels_seq +
        log(n_seq) * n_num_seq,
      data = results
    )

  # m7 but a bit more complex even
  m8 <-
    stats::lm(
      formula = log(time_median_ms) ~ log(n_seq) +
        log(n_seq) * n_fac_seq * n_fac_levels_seq +
        log(n_seq) * n_num_seq,
      data = results
    )

  # using a square root transformation
  m9 <-
    stats::lm(
      formula = sqrt(time_median_ms) ~ sqrt(n_seq) +
        n_fac_seq +
        n_fac_levels_seq +
        n_num_seq,
      data = results
    )

  # using a reciprocal transformation
  m10 <-
    stats::lm(
      formula = (1 / time_median_ms) ~ (1 / n_seq) +
        n_fac_seq +
        n_fac_levels_seq +
        n_num_seq,
      data = results
    )

  # using a reciprocal transformation
  m11 <-
    stats::lm(
      formula = (time_median_ms^2) ~ (n_seq^2) +
        n_fac_seq +
        n_fac_levels_seq +
        n_num_seq,
      data = results
    )

  # using log base 10
  m12 <-
    stats::lm(
      formula = log10(time_median_ms) ~ log10(n_seq) +
        n_fac_seq +
        n_fac_levels_seq +
        n_num_seq,
      data = results
    )

  # m8 but removing the first log(n_seq)
  m13 <-
    stats::lm(
      formula = log(time_median_ms) ~
        log(n_seq) * n_fac_seq:n_fac_levels_seq + log(n_seq):n_num_seq,
      data = results
    )

  # m8
  m14 <-
    stats::lm(
      formula = log(time_median_ms) ~ n_seq +
        n_seq:n_fac_seq:n_fac_levels_seq +
        n_seq:n_num_seq +
        n_fac_seq:n_num_seq,
      data = results
    )

  # combine to a list
  model_list <-
    list(
      m1 = m1,
      m2 = m2,
      m3 = m3,
      m4 = m4,
      m5 = m5,
      m6 = m6,
      m7 = m7,
      m8 = m8,
      m9 = m9,
      m10 = m10,
      m11 = m11,
      m12 = m12,
      m13 = m13,
      m14 = m14
    )
}

#' Minimise a linear model
#'
#' @description
#' Removes all components from a linear model that are not needed for prediction.
#'
#' @param m Linear regression model, as produced by stats::lm
#'
#' @returns Returns the input model but with multiple components removed
#'
#' @noRd
minimise_lm <- function(m) {
  # ensure sigma exists
  if (is.null(m$sigma) && !is.null(m$residuals)) {
    m$sigma <- sqrt(sum(m$residuals^2) / m$df.residual)
  }

  # what size is the model as passed in?
  bytes_in <- object.size(m)

  # reduce the size of the model by removing elements not required for prediction
  m$model <- NULL
  m$y <- NULL
  # m$residuals <- NULL
  m$fitted.values <- NULL
  m$effects <- NULL
  m$call <- NULL

  # what size is the model now?
  bytes_out <- object.size(m)

  # what is the conversion rate
  bytes_compression <- (1 - (as.numeric(bytes_out) / as.numeric(bytes_in)))

  # notify the user
  cli::cli_inform(
    c(
      "{.strong Model size report}",
      " Original size: {.emph {bytes_in |> prettyunits::pretty_bytes()}}",
      " Reduced size: {.emph {bytes_out |> prettyunits::pretty_bytes()}}",
      " Space saved: {.emph {bytes_compression |> scales::percent(accuracy = 0.1)}}"
    )
  )

  # return
  return(m)
}
