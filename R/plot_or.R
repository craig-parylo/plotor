# Set global variables ----
# set variables to avoid issue with dplyr and 'no visible global function definition for ...'
utils::globalVariables(
  base::c(
    # count_rows_by_variable
    'group', 'level', 'rows', 'term',

    # plot_odds_ratio
    'label_or', 'significance', 'rows_scale', 'conf.high', 'conf.low',
    'comparator', 'estimate',

    # use_var_labels
    'label'
  )
)

#' Plot OR
#'
#' Produces an Odds Ratio plot to visualise the results of a logistic regression analysis.
#'
#' @param glm_model_results Results from a binomial Generalised Linear Model (GLM), as produced by [stats::glm()].
#'
#' @return `plotor` returns an object of class `gg` and `ggplot`
#' @seealso
#' See vignette('using_plotor', package = 'plotor') for more details on use.
#'
#' More details and examples are found on the website: <https://craig-parylo.github.io/plotor/index.html>
#' @export
#' @examples
#' # libraries
#' library(plotor)
#' library(datasets)
#' library(dplyr)
#' library(ggplot2)
#' library(stats)
#' library(forcats)
#' library(tidyr)
#'
#' # get some data
#' df <- datasets::Titanic |>
#'   as_tibble() |>
#'   # convert aggregated counts to individual observations
#'   filter(n > 0) |>
#'   uncount(weights = n) |>
#'   # convert character variables to factors
#'   mutate(across(where(is.character), as.factor))
#'
#' # perform logistic regression using `glm`
#' lr <- glm(
#'   data = df,
#'   family = 'binomial',
#'   formula = Survived ~ Class + Sex + Age
#' )
#'
#' # produce the Odds Ratio plot
#' plot_or(lr)
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
      by = base::c('term')
    )

  # prepare the data for plotting
  df <- prepare_df_for_plotting(df = df)

  # use labels where provided
  df <- use_var_labels(df = df, lr = glm_model_results)

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
#' @return
#' Tibble summarising the number of rows of data in `df` for the variable `var_name`.
#' The class of the returned object is `tbl_df`, `tbl` and `data.frame`.
#'
#' @noRd
count_rows_by_variable <- function(df, var_name) {

  # prep
  var = base::as.symbol(var_name)

  var_temp <- df |>
    dplyr::select(tidyselect::all_of(var)) |>
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
#' @param model_results Results from a Generalised Linear Model (GLM) binomial model, as produced by [stats::glm()].
#'
#' @return Tibble summary of rows per variable used in the model
#' @noRd
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
#' @noRd
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
#' @param model Results from a Generalised Linear Model (GLM) binomial model, as produced by [stats::glm()].
#'
#' @return ggplot2 plot
#' @noRd
plot_odds_ratio <- function(df, model) {

  # get the name of the outcome variable - will be used in the plot title
  model_outcome_var = model$formula[[2]] |> base::as.character()
  model_outcome_label = base::sapply(model$data[model_outcome_var], function(x){base::attr(x,"label")})[[1]]
  model_outcome = dplyr::coalesce(model_outcome_label, model_outcome_var |> base::as.character())

  # plot the OR plot using ggplot2
  df |>
    dplyr::arrange(dplyr::desc(estimate)) |>
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
#' A custom labeller function for use in the odds-ratio plot.
#'
#' The function ensures factor groups are listed for the first instance of each level and as empty strings for any subsequent level. It is intended to reduce the visual clutter in the odds-ratio plot.
#'
#' @param group Character vector of groups
#' @param level Character vector of factor levels
#'
#' @return Character vectors of labels for both group and level
#' @noRd
label_groups <- function(group, level) {
  dplyr::case_when(
    dplyr::row_number(group) == 1 ~ group,
    !group == dplyr::lag(group) ~ group,
    .default = ''
  )
}

#' Use Variable Labels
#'
#' Where variables have been given a label attribute then the label is used in the plot
#'
#' @param df Tibble of data expanded with variables to aid plotting - as outputted from [prepare_df_for_plotting()]
#' @param lr Results from a Generalised Linear Model (GLM) binomial model, as produced by [stats::glm()].
#'
#' @return Tibble of data with group labels used where available
#' @noRd
use_var_labels <- function(df, lr) {

  # get the data from lr
  df_model <- lr$data

  # get the variable labels as a list
  vars_labels = base::sapply(df_model, function(x){base::attr(x,"label")})

  # convert to a tibble and handle any missing labels
  vars_labels = dplyr::tibble(
    group = base::names(vars_labels),
    label = vars_labels |> base::as.character()
  ) |>
    dplyr::mutate(label = label |> dplyr::na_if(y = 'NULL'))

  # left join the labels to the df, change group to match the label (where available)
  df <- df |>
    dplyr::left_join(
      y = vars_labels,
      by = 'group'
    ) |>
    dplyr::mutate(group = dplyr::coalesce(label, group)) |>
    dplyr::select(-label)

  return(df)

}
