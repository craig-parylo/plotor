# -----------------------------------------------------------------------------
# BENCHMARKING PROCESSING TIME
#
# This script is used to train a model that predicts the time to run
# `plotor::plot_or()` using the default method for calculating confidence
# intervals.
#
# This model will be used to alert users when a long processing time is
# expected so they can switch to the faster method of confidence interval
# calculation
# ----------------------------------------------------------------------------

# load the functions
here::here("data-raw", "benchmark_funcs.R") |> source()

# load the data
results <- get_benchmark_results()

# collate some candidate models
model_list <- collate_model_candidates(results)

# test the models
test <- assess_models(model_list = model_list, df_results = results)

# m8 is the best performing model, select it as the preferred model
model_time_taken <- model_list$m8

# reduce the size of the object
model_time_taken <- minimise_lm(model_time_taken)

# store the model for use in the app
usethis::use_data(
  model_time_taken,
  internal = TRUE,
  compress = "xz",
  overwrite = TRUE
)

# predict time taken
predict(
  object = model_time_taken,
  newdata = tibble::tibble(
    n_seq = 300000L,
    n_fac_seq = 4L,
    n_fac_levels_seq = 3L,
    n_num_seq = 2L
  ),
  interval = "prediction",
  level = 0.95
) |>
  exp() |>
  max()


predict(
  object = model_time_taken,
  newdata = tibble::tibble(
    n_seq = 300000L,
    n_fac_seq = 2L,
    n_fac_levels_seq = 5L,
    n_num_seq = 0L
  ),
  interval = "prediction",
  level = 0.95
) |>
  exp() |>
  max() |>
  floor()

# lots of scratchpad activity ------------------------

# m1 <-
#   stats::lm(
#     formula = log(time_median_ms) ~ n_seq +
#       n_fac_seq +
#       n_fac_levels_seq +
#       n_num_seq,
#     data = results
#   )

# model_list <- list(m1 = m1)

# performance::check_model(m1)
# performance::model_performance(model_list)

# assess_models(model_list = model_list, df_results = results)

# performance::check_model(m8)

# # what distribution for outcome
# results |>
#   ggplot2::ggplot(ggplot2::aes(x = poly(n_num_seq, df = 2))) +
#   ggplot2::geom_density()

# results |>
#   ggplot2::ggplot(ggplot2::aes(x = log(time_median_ms, base = 4))) +
#   ggplot2::geom_density()

# results |>
#   dplyr::filter(n_fac_seq == 5, n_fac_levels_seq == 2, n_num_seq == 1) |>
#   # ggplot2::ggplot(ggplot2::aes(x = log(n_seq), y = log(time_median_ms))) +
#   ggplot2::ggplot(ggplot2::aes(x = log(n_seq), y = log(time_median_ms))) +
#   ggplot2::geom_point() +
#   ggplot2::geom_smooth(method = "loess", se = FALSE, colour = "blue")

# # does assumption of linearity hold?
# # plot each predictor vs outcome
# results |>
#   tidyr::pivot_longer(
#     cols = -time_median_ms,
#     names_to = "predictor",
#     values_to = "value"
#   ) |>
#   ggplot2::ggplot(ggplot2::aes(x = value, y = time_median_ms)) +
#   ggplot2::geom_point(alpha = 0.4) +
#   ggplot2::geom_smooth(method = "loess", se = FALSE, colour = "red") +
#   ggplot2::facet_wrap(~predictor, scales = "free_x")
