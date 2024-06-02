#' Plot OR
#'
#' Produces an Odds Ratio plot to visualise the results of a logistic regression analysis.
#'
#' @param glm_model_results Results from a GLM binomial model results
#'
#' @return ggplot2-produced visualisation
#' @export
#' @import dplyr
#' @import broom
#' @import here
#'
plot_or <- function(glm_model_results) {

  # get the data from the model object
  df <- summarise_rows_per_variable_in_model(model_results = glm_model_results)

  # get odds ratio and confidence intervals
  model_or <- glm_model_results |>
    broom::tidy(exponentiate = T, conf.int = T)

  # add the odds ratio and CIs to the summary dataframe
  df <- df |>
    dplyr::left_join(
      y = model_or,
      by = c('term')
    )

  # prepare the data for plotting
  df <- prepare_df_for_plotting(df = df)

  # plot the results
  p <- plot_odds_ratio(df = df, model = glm_model_results)

  return(p)
}

#' Count Rows by Variable
#'
#' Takes a tibble of data and a string name of a variable in the tibble and returns a count of rows.
#'
#' For numeric variables the result is a count of all non-NA rows.
#'
#' For character and factor variables the result is a count of all non-NA rows per level / value of character.
#'
#' @param df Tibble of data
#' @param var_name String name of the variable to count
#'
#' @return Tibble summarising the number of rows of data in var in total if var is numeric or by each level if var is a character
#' @export
#' @import dplyr
#' @import tidyselect
count_rows_by_variable <- function(df, var_name) {

  # prep
  var = base::as.symbol(var_name)

  var_temp <- df |>
    dplyr::select(tidyselect::any_of(var)) |>
    dplyr::pull()

  # calculate rows - split if categorical
  if(is.numeric(var_temp)) {
    df |>
      dplyr::filter(!is.na(var_name)) |>
      dplyr::summarise(rows = dplyr::n()) |>
      dplyr::mutate(group = var_name, level = var_name, term = var_name) |>
      dplyr::select(term, group, level, rows)
  } else {
    df |>
      dplyr::summarise(rows = dplyr::n(), .by = var) |>
      dplyr::rename(level = var) |>
      dplyr::mutate(group = var_name, term = base::paste0(group, level)) |>
      dplyr::select(term, group, level, rows)
  }
}

#' Get Summary of Rows per Variable in Model
#'
#' Returns a summary of the number of rows per variable used in the model.
#'
#' @param model_results
#'
#' @return Tibble summary of rows per variable used in the model
#' @import dplyr
#' @import stats
#' @import purrr
#' @import scales
summarise_rows_per_variable_in_model <- function(model_results) {

  # get the data from the model object
  model_data <- model_results$model |> dplyr::as_tibble()

  # get the model variables
  model_vars = base::all.vars(stats::formula(model_results)[-2])

  # get a summary of all model vars (will be used as the spine of the data)
  # this is important because the first value of factors is used as the baseline
  # in the OR analysis and can be missed in the glm summary.
  df <- model_vars |>
    purrr::map_dfr(\(.x) count_rows_by_variable(df = model_data, var_name = .x)) |>
    # rescale rows (will be used to set the size of the dot in the plot)
    dplyr::mutate(rows_scale = rows |> scales::rescale(to = c(1, 5)))

  # return the table summary
  return(df)
}

#' Prepare dataframe for plotting
#'
#' @param df Tibble of data combining OR estimates and counts of rows per variables
#'
#' @return Tibble of data expanded with variables to aid plotting
prepare_df_for_plotting <- function(df) {

  df <- df |>
    dplyr::mutate(

      # flag records which do not cross the line of no effect
      significance = dplyr::case_when(
        base::is.na(estimate) ~ 'Comparator',
        (conf.low < 1) & (conf.high < 1) ~ 'Significant',
        (conf.low > 1) & (conf.high > 1) ~ 'Significant',
        .default = 'Not significant'
      ),

      # set all comparator groups at one as a baseline
      comparator = dplyr::case_when(
        base::is.na(estimate) ~ 1
      ),

      # probability label - helper for the below 'label_or' step
      p_label = dplyr::case_when(
        base::is.na(p.value) ~ NA,
        p.value < 0.001 ~ 'p<0.001',
        .default = glue::glue('p={round(p.value, digits = 3)}')
      ),

      # produce a label for each OR result
      label_or = dplyr::case_when(
        base::is.na(estimate) ~ glue::glue('{group} (comparator)'),
        .default = glue::glue(
          '{round(estimate, digits = 2)} ', # OR estimate
          '({round(conf.low, digits = 2)}-', # lower CI
          '{round(conf.high, digits = 2)}, ', # upper CI
          '{p_label})' # probability
        )
      )
    )
}

#' Plot the Odds Ratio
#'
#' Plot the OR plot using ggplot2 using points for OR estimates and whiskers for confidence intervals.
#'
#' The plot is faceted on variables with sub-levels shown where the variable is a factor.
#'
#' @param df Tibble of data containing a pre-prepared OR object
#' @param model A glm logistic regression model
#'
#' @import dplyr
#' @import ggplot2
#' @import glue
#'
#' @return ggplot2 plot
plot_odds_ratio <- function(df, model) {

  # get the name of the outcome variable - will be used in the plot title
  model_outcome = model$formula[[2]]

  # plot the OR plot using ggplot2
  df |>
    dplyr::group_by(group) |>
    ggplot2::ggplot(ggplot2::aes(y = label_or, x = estimate, colour = significance)) +
    ggplot2::facet_grid(
      rows = dplyr::vars(group, level),
      scales = 'free_y',
      space = 'free_y',
      switch = 'y',
      labeller = ggplot2::labeller(group = label_groups)
    ) +
    ggplot2::geom_vline(xintercept = 1, linetype = 'dotted') +
    # plot the OR with 95% CI
    ggplot2::geom_point(ggplot2::aes(size = rows_scale), shape = 15) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmax = conf.high, xmin = conf.low), height = 1/5) +
    # plot the comparator levels
    ggplot2::geom_point(
      ggplot2::aes(y = label_or, x = comparator, colour = significance, size = rows_scale),
      shape = 15
    ) +
    ggplot2::scale_x_log10(n.breaks = 10) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      # position the title to the far left
      plot.title.position = 'plot',

      # clean up the plot some more
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = 'none',
      axis.title.y = ggplot2::element_blank(),

      # handle groups
      panel.spacing = ggplot2::unit(0, 'lines'),
      strip.placement = 'outside',
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, vjust = 0.5)
    ) +
    ggplot2::labs(
      title = glue::glue('{model_outcome}'),
      subtitle = 'Odds Ratio (OR) plot with 95% Confidence Interval (CI)',
      x = 'Odds ratio (95% CI, log scale)'
    ) +
    ggplot2::scale_colour_manual(values = c(
      'Significant' = '#192a56',
      'Comparator' = '#718093',
      'Not significant' = '#487eb0')
    )
}

#' Labeller for groups
#'
#' A custom labeller function for use in the OR plot.
#'
#' It ensures factor groups are listed for the first instance of each level and as empty strings for any subsequent level. It is intended to reduce the visual clutter in the OR plot.
#'
#' @param group Character vector of groups
#' @param level Character vector of factor levels
#'
label_groups <- function(group, level) {
  dplyr::case_when(
    dplyr::row_number(group) == 1 ~ group,
    !group == dplyr::lag(group) ~ group,
    .default = ''
  )
}
