make_tibble <- function(
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

  # combine the elements
  # df_return <-
  #     tibble::tibble(outcome = outcome) |>
  #     dplyr::bind_cols(fac_df, num_df)

  # return the result
  return(df_return)
}

# create a model based on test
make_model_from_tibble <- function(df) {
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

df_params <-
  tidyr::crossing(
    n_seq = seq(from = 100L, to = 1e3, by = 100L),
    n_fac_seq = seq(from = 1, to = 50L, by = 10L),
    n_fac_levels_seq = seq(from = 2, to = 50L, by = 10L),
    n_num_seq = seq(from = 1, to = 50L, by = 10L)
  )

benchmark_results <- function() {
  # get a tibble of parameters
  df_params <-
    tidyr::crossing(
      n_seq = seq(from = 100L, to = 200L, by = 100L),
      n_fac_seq = seq(from = 1, to = 10L, by = 2L),
      n_fac_levels_seq = seq(from = 2, to = 5L, by = 1L),
      n_num_seq = seq(from = 1, to = 10L, by = 2L)
    )

  df_return <-
    df_params |>
    dplyr::mutate(
      mb = purrr::pmap(
        .progress = TRUE,
        .l = list(n_seq, n_fac_seq, n_fac_levels_seq, n_num_seq),
        .f = ~ {
          # create the model data
          model_data <- make_model_tibble(
            n = ..1,
            n_fac = ..2,
            n_fac_levels = ..3,
            n_num = ..4
          )

          # create the model
          model <- make_model_from_tibble(df = model_data)

          # test how long it takes
          result <-
            microbenchmark::microbenchmark(
              plotor::plot_or(
                glm_model_results = model,
                confint_fast_estimate = FALSE,
                assumption_checks = FALSE
              ),
              times = 2
            )

          # average the time taken
          time <- result$time |> median() / 1e6 # convert from microseconds to milliseconds
        }
      )
    ) |>
    # mb is a list-column of numeric values
    dplyr::mutate(time_median_ms = purrr::map_dbl(mb, identity)) |>
    dplyr::select(-mb)
}
results <- benchmark_results()

test_dat <-
  make_tibble(
    n = 100,
    n_fac = 1,
    n_fac_levels = 2,
    n_num = 1
  )

test_model <-
  make_model_from_tibble(df = test_dat)

names(test_dat)

result <- microbenchmark::microbenchmark(
  output <-
    plotor::plot_or(
      glm_model_results = test_model,
      confint_fast_estimate = FALSE,
      assumption_checks = FALSE,
      conf_level = 0.95
    ),
  times = 10
)

result$time |> median() / 1e6

test <- make_tibble(n = 1e5, n_fac = 2, n_fac_levels = 5, n_num = 2)
lr <- make_model_from_tibble(df = test)
plotor::plot_or(lr, assumption_checks = FALSE)


n_seq <- seq(from = 100L, to = 1e4, by = 50L)
n_fac_seq <- seq(from = 0, to = 100L, by = 1L)
n_fac_levels_seq <- seq(from = 2, to = 100L, by = 1L)
n_num_seq <- seq(from = 0, to = 100L, by = 1L)
