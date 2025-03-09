# Exported functions -----------------------------------------------------------

#' Plot OR
#'
#' Produces an Odds Ratio plot to visualise the results of a logistic regression analysis.
#'
#' @param glm_model_results Results from a binomial Generalised Linear Model (GLM), as produced by [stats::glm()].
#' @param conf_level Numeric between 0.001 and 0.999 (default = 0.95). The confidence level to use when setting the confidence interval, most commonly will be 0.95 or 0.99 but can be set otherwise.
#'
#' @return an object of class `gg` and `ggplot`
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
plot_or <- function(glm_model_results, conf_level = 0.95) {

  # data and input checks ----
  # check the model is logistic regression
  valid_glm_model <- validate_glm_model(glm_model_results)

  # check logistic regression assumptions
  valid_assumptions <- check_assumptions(glm = glm_model_results, details = FALSE)

  # limit conf_level to between 0.001 and 0.999
  conf_level <- validate_conf_level_input(conf_level)

  # main ----

  # get summary of the data and results
  df <- get_summary_table(glm_model_results = glm_model_results, conf_level = conf_level)

  # plot the results
  p <- plot_odds_ratio(df = df, model = glm_model_results, conf_level = conf_level)

  return(p)
}

#' Table OR
#'
#' Produces a formatted table showing the outputs from the Odds Ratio analysis.
#'
#' Includes details on the characteristics of the covariates, such as:
#' * the number of observations for each characteristic,
#' * the number of observations resulting in the outcome of interest,
#' * the conversion rate of outcome by the number of observations,
#'
#' Details are calculated showing the:
#' * estimated Odds Ratio, standard error and p-value,
#' * calculated confidence interval for the confidence level,
#'
#' Also included is a visualisation of the OR plot to provide an at-a-glance
#' view of the model results.
#'
#' @param glm_model_results Results from a binomial Generalised Linear Model (GLM), as produced by [stats::glm()].
#' @param conf_level Numeric between 0.001 and 0.999 (default = 0.95). The confidence level to use when setting the confidence interval, most commonly will be 0.95 or 0.99 but can be set otherwise.
#' @param output String description of the output type. Default = 'tibble'. Options include 'tibble' and 'gt'.
#'
#' @returns object returned depends on `output` parameter - output = 'tibble' returns an object of "tbl_df", "tbl", "data.frame" class, whilst output = 'gt' returns an object of class "gt_tbl" and "list".
#' @export
#'
#' @examples
#' # get some data
#' df <- datasets::Titanic |>
#'   dplyr::as_tibble() |>
#'   # convert aggregated counts to individual observations
#'   dplyr::filter(n > 0) |>
#'   tidyr::uncount(weights = n) |>
#'   # convert character variables to factors
#'   dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))
#'
#' # perform logistic regression using `glm`
#' lr <- stats::glm(
#'   data = df,
#'   family = 'binomial',
#'   formula = Survived ~ Class + Sex + Age
#' )
#'
#' # produce the Odds Ratio table, first as a tibble then as gt object
#' table_or(lr)
#' table_or(lr, output = 'gt')
table_or <- function(glm_model_results, conf_level = 0.95, output = 'tibble') {

  # data and input checks ----
  # check the model is logistic regression
  valid_glm_model <- validate_glm_model(glm_model_results)

  # check logistic regression assumptions
  valid_assumptions <- check_assumptions(glm = glm_model_results, details = FALSE)

  # limit conf_level to between 0.001 and 0.999
  conf_level <- validate_conf_level_input(conf_level)

  # limit output to acceptable types and raise an error if not
  output_valid <- validate_output_table_input(output)

  # main ----
  # get summary of rows and estimate OR
  df <- get_summary_table(glm_model_results = glm_model_results, conf_level = conf_level)

  # get the outcome variable
  str_outcome <- get_outcome_variable_name(model = glm_model_results)

  # prepare for output
  df <-
    df |>
    # remove variables which aren't necessary for table views
    dplyr::select(!dplyr::any_of(c(
      'term', 'rows_scale', 'label_or', 'group', 'p_label'
    ))) |>
    # work out the rate of 'outcome'
    dplyr::mutate(outcome_rate = .data$outcome / .data$rows) |>
    dplyr::relocate('outcome_rate', .after = 'outcome')

  # decide what object to return
  obj_return <-
    switch(output,
      # output a tibble
      'tibble' = {df},

      # output a gt-formatted table
      'gt' = {
        df |>
          dplyr::group_by(.data$label) |>
          output_gt(conf_level = conf_level, title = str_outcome)
        }
    )

  return(obj_return)
}

# Internal functions -----------------------------------------------------------
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
count_rows_by_variable <- function(df, var_name, outcome_name) {
  # prep
  var = base::as.symbol(var_name)
  outcome = base::as.symbol(outcome_name)

  # determine the outcome of interest
  outcome_num <- df[[outcome_name]] |> stats::na.omit() |> as.numeric() |> max(na.rm = TRUE)
  outcome_txt <- levels(df[[outcome_name]])[outcome_num]

  var_temp <- df |>
    dplyr::select(tidyselect::all_of(var)) |>
    dplyr::pull()

  # calculate rows - split if categorical
  df <-
    if (is.numeric(var_temp)) {
      df |>
        dplyr::filter(!is.na(var_name)) |>
        dplyr::summarise(rows = dplyr::n(), outcome = sum({{outcome}} == outcome_txt)) |>
        dplyr::mutate(group = var_name,
                      level = var_name,
                      term = var_name) |>
        dplyr::select(dplyr::any_of(c(
          'term', 'group', 'level', 'rows', 'outcome'
        )))
    } else {
      df |>
        dplyr::mutate(outcome = sum({{outcome}} == outcome_txt), .by = {{var}}) |>
        #dplyr::summarise(rows = dplyr::n(), .by = c({{var}}, {{outcome}})) |>
        dplyr::summarise(rows = dplyr::n(), .by = c({{var}}, 'outcome')) |>
        dplyr::rename(level = {{var}}) |>
        dplyr::mutate(group = var_name,
                      term = base::paste0(.data$group, .data$level)) |>
        dplyr::select(dplyr::any_of(c(
          'term', 'group', 'level', 'rows', 'outcome'
        )))
    }

  # add the class of the variable
  df <-
    df |>
    dplyr::mutate(class = class(var_temp))

  return(df)
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

  # get the outcome variable
  model_outcome = base::all.vars(stats::formula(model_results))[1]

  # get a summary of all model vars (will be used as the spine of the data)
  # this is important because the first value of factors is used as the baseline
  # in the OR analysis and can be missed in the glm summary.
  df <-
    model_vars |>
    purrr::map_dfr(\(.x) count_rows_by_variable(
      df = model_data,
      var_name = .x,
      outcome_name = model_outcome
    )) |>
    # rescale rows (will be used to set the size of the dot in the plot)
    #dplyr::mutate(rows_scale = .data$rows |> scales::rescale(to = c(1, 5)))
    dplyr::mutate(
      rows_scale = dplyr::case_when(
          .data$class == 'numeric' ~ 1,
          .default = .data$rows |>
            scales::rescale(to = c(1, 5))
        )
    )

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
      comparator = dplyr::case_when(base::is.na(estimate) ~ 1),

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
          '{round(estimate, digits = 2)} ',
          # OR estimate
          '({round(conf.low, digits = 2)}-',
          # lower CI
          '{round(conf.high, digits = 2)}, ',
          # upper CI
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
#' @param conf_level Numeric value below 1 indicating the confidence level used when producing the Confidence Interval
#'
#' @return ggplot2 plot
#' @noRd
plot_odds_ratio <- function(df, model, conf_level) {
  # get the name of the outcome variable - will be used in the plot title
  model_outcome <- get_outcome_variable_name(model = model)

  # get the confidence level as string
  str_conf_level <- glue::glue('{conf_level * 100}%')

  # plot the OR plot using ggplot2
  df |>
    ggplot2::ggplot(ggplot2::aes(
      y = .data$label_or,
      x = .data$estimate,
      colour = .data$significance
    )) +
    ggplot2::facet_grid(
      rows = dplyr::vars(.data$label, .data$level),
      scales = 'free_y',
      space = 'free_y',
      switch = 'y',
      labeller = ggplot2::labeller(label = label_groups, .multi_line = TRUE)
    ) +
    ggplot2::geom_vline(xintercept = 1, linetype = 'dotted') +
    # plot the OR with 95% CI
    ggplot2::geom_point(
      # replace any NA `estimate` and `rows_scale` with nominal to
      # avoid warnings about missing values
      data = df |>
        dplyr::mutate(
          estimate = dplyr::coalesce(.data$estimate, 1),
          rows_scale = dplyr::coalesce(.data$rows_scale, 0)
        ),
      ggplot2::aes(size = .data$rows_scale),
      shape = 15
    ) +
    ggplot2::geom_errorbarh(
      # remove any confidence estimates with NA values
      data = df |> dplyr::filter(!is.na(.data$conf.high), !is.na(.data$conf.low)),
      ggplot2::aes(xmax = .data$conf.high, xmin = .data$conf.low),
      height = 1 / 5
    ) +
    ggplot2::scale_x_log10(n.breaks = 10, labels = scales::comma) +
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
      strip.text.y.left = ggplot2::element_text(
        angle = 0,
        hjust = 1,
        vjust = 0.5
      )
    ) +
    ggplot2::labs(
      title = glue::glue('{model_outcome}'),
      subtitle = glue::glue('Odds Ratio (OR) plot with {str_conf_level} Confidence Interval (CI)'),
      x = glue::glue('Odds ratio ({str_conf_level} CI, log scale)')
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        'Significant' = '#192a56',
        'Comparator' = '#718093',
        'Not significant' = '#487eb0'
      )
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
  dplyr::case_when(is.na(dplyr::lag(group)) ~ group,
                   group != dplyr::lag(group) ~ group,
                   .default = '')
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
  vars_labels = base::sapply(df_model, function(x) {
    base::attr(x, "label")
  })

  # create a tibble of correctly ordered group variable and labels
  df_vars_labels <-
    dplyr::tibble(
      # define group based on the variable labels
      group = base::names(vars_labels) |> # set 'group' to be the names of the variable
        factor(levels = labels(terms(lr))),
      # set levels to suit glm's model terms
      # capture the descriptive labels
      label = vars_labels |>
        base::as.character() |> # need character for next step
        dplyr::na_if(y = 'NULL') |> # if there are no labels then set as NA
        dplyr::coalesce(.data$group) |> # replace NA with group name
        stringr::str_wrap(width = 15) |> # wrap longer length labels
        forcats::fct() |> # convert all to a factor
        forcats::fct_reorder(as.numeric(.data$group), .na_rm = TRUE) # order to suit the group
    )

  # right join the df to df_vars_labels to integrate the new factors in the df
  # NB, right join to exclude labels for the outcome variable
  df_return <-
    df_vars_labels |>
    dplyr::right_join(
      y = df |>
        dplyr::rename(group_old = "group"),
      by = dplyr::join_by('group' == 'group_old')
    ) |>
    dplyr::arrange('label', 'level')

  return(df_return)

}

## validation funcs -----
#' Validate confidence level input
#'
#' Checks the parameter for the confidence level is within accepted limits.
#' For example a value of 1 (to be 100% confident) is not possible to calculate,
#' so this function checks the input against upper and lower limits and adjusts
#' the value to sit within accepted limits.
#'
#' @param conf_level Numeric input from the user
#'
#' @returns Numeric value between upper and lower limits
#' @noRd
validate_conf_level_input <- function(conf_level) {
  # set limits
  ci_min <- 0.001
  ci_max <- 0.999

  # handle inputs of multiple lengths
  if (length(conf_level) > 1) {
    cli::cli_abort(c(
      "{.var conf_level} requires a single numeric value.",
      "x" = "There are {?is/are} {length(conf_level)} element{?s} in the value you supplied."
    ))
  }

  # handle non-numeric inputs
  if (!is.numeric(conf_level)) {
    cli::cli_abort(c(
      "{.var conf_level} requires a numeric value.",
      "x" = "The supplied value {.val {conf_level}} is a {.type conf_level} input."
    ))
  }

  # validate conf_level
  conf_level_new <- dplyr::case_when(

    # parse if given an integer version, e.g. 95 instead of 0.95
    conf_level > 50 & conf_level < 100 ~ conf_level / 100,

    # if otherwise outside limits then set to nearest limit
    conf_level < ci_min | conf_level > ci_max ~ min(ci_max, max(ci_min, conf_level)),

    # if all checks pass then return the input
    .default = conf_level
  )

  # let the user know what is happening
  if (conf_level < ci_min) {
    cli::cli_alert_info(
      "{.var conf_level} = {.val {conf_level}} is below the minimum accepted value.
      Setting to {.val {conf_level_new}} instead."
    )
  } else if (conf_level > ci_max) {
    cli::cli_alert_info(
      "{.var conf_level} = {.val {conf_level}} is above the maximum accepted value.
      Setting to {.val {conf_level_new}} instead."
    )
  }

  return(conf_level_new)
}

#' Validate the `{glm}` model
#'
#' Check whether the glm model object is the product of logistic regression.
#'
#' @param glm_model Results from a binomial Generalised Linear Model (GLM), as produced by [stats::glm()]
#'
#' @returns boolean (TRUE = logistic regression, FALSE = other model)
#' @noRd
validate_glm_model <- function(glm_model) {

  # find the response to the validation
  response <- (
    class(glm_model)[1] == 'glm' & # must be a glm class object
      glm_model$family$family == 'binomial' & # must be a binomial model
      glm_model$family$link == 'logit' # must use logit link
  )

  # if fail validation then message the user
  if (!response) {
    glm_family <- glm_model$family$family
    cli::cli_abort(
      message = c(
        "{.arg glm_model_results} must be a {.pkg glm} object of {.val binomial} family.",
        "You've supplied a {.val {glm_family}} family model."
      )
    )
  }

  return(response)
}

#' Validate the 'output' parameter
#'
#' Check the requested 'output' matches one of the accepted 'output' types.
#'
#' @param output String description of the output type. Default = 'tibble'. Options include 'tibble' and 'gt'.
#'
#' @returns Boolean indicating whether the 'output' parameter is valid
#' @noRd
validate_output_table_input <- function(output) {

  # specify accepted output types
  accepted_outputs <- c('tibble', 'gt')

  # do some basic input cleaning
  output <- output |> trimws() |> tolower()

  # record the result of the check
  result <- output %in% accepted_outputs

  # message the user if any issues
  if (!result) {
    cli::cli_abort(
      message = c(
        "{.arg output} must be one of {.or {.val {accepted_outputs}}}.",
        "You've requested an output of {.val {output}}."
      )
    )
  }

  return(result)
}

## output tables ----

#' Get a table summarising the model results
#'
#' Get a summary table showing the number of rows in each group and of those
#' who and a 'success' outcome. Then combine with details such as the OR
#' estimate and confidence interval.
#'
#' @param glm_model_results Results from a binomial Generalised Linear Model (GLM), as produced by [stats::glm()].
#' @param conf_level Numeric between 0.001 and 0.999 (default = 0.95). The confidence level to use when setting the confidence interval, most commonly will be 0.95 or 0.99 but can be set otherwise.
#'
#' @returns Tibble providing a summary of the logistic regression model.
#' @noRd
get_summary_table <- function(glm_model_results, conf_level) {
  # get the data from the model object
  df <- summarise_rows_per_variable_in_model(model_results = glm_model_results)

  # get odds ratio and confidence intervals
  model_or <- glm_model_results |>
    broom::tidy(exponentiate = T,
                conf.int = T,
                conf.level = conf_level)

  # add the odds ratio and CIs to the summary dataframe
  df <- df |>
    dplyr::left_join(y = model_or, by = base::c('term'))

  # use variable labels
  df <- use_var_labels(df = df, lr = glm_model_results)

  # prepare the data for plotting
  df <- prepare_df_for_plotting(df = df)

  # return the df
  return(df)
}

#' Output tibble as `gt`
#'
#' Outputs a publication-quality summary OR table with {gt} formatting.
#'
#' @param df Tibble of summary data produced by `table_or()`
#' @param conf_level Numeric between 0.001 and 0.999 (default = 0.95). The confidence level to use when setting the confidence interval, most commonly will be 0.95 or 0.99 but can be set otherwise.
#'
#' @returns {gt}
#' @noRd
output_gt <- function(df, conf_level, title = "Odds Ratio Summary Table") {

  # get the outcome

  # produce the gt table
  tab_gt <-
    df |>
    # log the OR and CI for plotting
    dplyr::mutate(
      plot_or = log(.data$estimate),
      plot_ci_l = log(.data$conf.low),
      plot_ci_u = log(.data$conf.high)
    ) |>
    # prepare for tabulation
    gt::gt(row_group_as_column = TRUE) |>
    # column labels
    gt::cols_label(
      label = 'Variable',
      level = 'Level',
      rows = 'N',
      outcome = 'n',
      outcome_rate = 'Rate',
      class = 'Class',
      estimate = 'OR',
      std.error = 'SE',
      p.value = 'p',
      conf.low = 'Lower',
      conf.high = 'Upper',
      significance = 'Significance',
      plot_or = 'OR Plot'
    ) |>
    # column formats
    gt::fmt_integer(
      columns = c(.data$rows, .data$outcome),
      use_seps = TRUE
    ) |>
    gt::fmt_percent(
      columns = c(.data$outcome_rate),
      decimals = 2,
      drop_trailing_zeros = TRUE
    ) |>
    gt::fmt_number(
      columns = c(.data$estimate, .data$std.error, .data$conf.low, .data$conf.high),
      n_sigfig = 4
    ) |>
    gt::fmt_scientific(
      columns = c(.data$p.value),
      n_sigfig = 3
    ) |>
    # spanners
    gt::tab_spanner(
      label = 'Characteristic',
      columns = c(.data$label:.data$class)
    ) |>
    gt::tab_spanner(
      label = 'Odds Ratio (OR)',
      columns = c(.data$estimate, .data$std.error, .data$statistic, .data$p.value),
      id = 'or'
    ) |>
    gt::tab_spanner(
      label = glue::glue('{conf_level * 100}% Confidence Interval (CI)'),
      columns = c(.data$conf.low, .data$conf.high, .data$significance),
      id = 'ci'
    ) |>
    # reference value rows
    gt::sub_missing() |>
    # hide columns that don't need displaying
    gt::cols_hide(columns = c(
      .data$comparator,
      .data$statistic,
      .data$plot_ci_l,
      .data$plot_ci_u
    )) |>
    # add titles
    gt::tab_header(
      title = gt::md(glue::glue("{title}")),
      subtitle = gt::md(glue::glue("Odds Ratio summary table with {conf_level * 100}% Confidence Interval"))
    ) |>
    # add footnotes
    gt::tab_footnote(
      locations = gt::cells_column_spanners('Characteristic'),
      footnote = gt::md("**Characteristics** are the explanatory variables in the logistic regression analysis. For categorical variables the first characteristic is designated as a reference against which the others are compared. For numeric variables the results indicate a change per single unit increase.\n\n
*Level* - the name or the description of the explanatory variable.\n\n
*N* - the number of observations examined.\n\n
*n* - the number of observations resulting in the outcome of interest.\n\n
*Rate* - the proportion of observations resulting in the outcome of interest (n / N).\n\n
*Class* - description of the data type.")
    ) |>
    gt::tab_footnote(
      locations = gt::cells_column_spanners('or'),
      footnote = gt::md("**Odds Ratios** estimate the relative *odds* of an outcome with reference to the *Characteristic*. For categorical data the first level is the reference against which the odds of other levels are compared. Numerical characteristics indicate the change in *OR* for each additional increase of one unit in the variable.\n\n
*OR* - The Odds Ratio point estimate - values below 1 indicate an inverse relationship whereas values above 1 indicate a positive relationship. Values shown to 4 significant figures.\n\n
*SE* - Standard Error of the point estimate. Values shown to 4 significant figures.\n\n
*p* - The p-value estimate based on the residual Chi-squared statistic.")
    ) |>
    gt::tab_footnote(
      locations = gt::cells_column_spanners('ci'),
      footnote = gt::md(glue::glue("**Confidence Interval** - the range of values likely to contain the *OR* in {conf_level * 100}% of cases if this study were to be repeated multiple times. If the *CI* touches or crosses the value 1 then it is unlikely the *Characteristic* is significantly associated with the outcome.\n\n
*Lower* & *Upper* - The range of values comprising the *CI*, shown to 4 significant figures.\n\n
*Significance* - The statistical significance indicated by the *CI*, *Significant* where the *CI* does not touch or cross the value 1.
      "))
    ) |>
    # add an OR plot to visualise the results
    gtExtras::gt_plt_conf_int(
      column = 'plot_or',
      ci_columns = c('plot_ci_l', 'plot_ci_u'),
      ref_line = 0,
      text_size = 0
    ) |>
    gt::cols_align(
      columns = .data$plot_or,
      align = 'center'
    )

}


#' Get the outcome of interest
#'
#' Returns a string description of the outcome of interest in the model.
#'
#' @param model Results from a binomial Generalised Linear Model (GLM), as produced by [stats::glm()].
#'
#' @returns String
#' @noRd
get_outcome_variable_name <- function(model, return_var_name = FALSE) {

  # get the name of the outcome variable from the model formula
  model_outcome_var <-
    model$formula[[2]] |>
    base::as.character()

  # get any label associated with this outcome variable
  model_outcome_label <-
    base::sapply(model$data[model_outcome_var], function(x) {
      base::attr(x, "label")
    })[[1]]

  # return either the label or variable name
  model_outcome <-
    dplyr::coalesce(model_outcome_label,
                    model_outcome_var |> base::as.character())

  # return the variable name if requested
  if (return_var_name) {
    return(model_outcome_var)
  } else {
    return(model_outcome)
  }

}


## assumptions funcs -----------------------------------------------------------

#' Check assumptions
#'
#' Checks whether the supplied `glm` model satisfies assumptions of a binary
#' logistic regression model.
#'
#' The assumptions tested are:
#' * the outcome variable is binary encoded,
#'
#' @param glm Results from a binomial Generalised Linear Model (GLM), as produced by [stats::glm()].
#' @param details Boolean: TRUE = additional details will be printed to the Console if this assumption fails, FALSE = additional details will be suppressed.
#'
#' @returns Named list indicating the results of each assumption
#' @noRd
check_assumptions <- function(glm, details = FALSE) {

  # check assumptions
  list_return <- list(
    assume_binary = assumption_binary_outcome(glm = glm, details = details),
    assume_independent = assumption_no_multicollinearity(glm = glm, details = details)
  )

  # aborting assumptions
  if (list_return$assume_binary != TRUE) {
    cli::cli_abort("Assumptions required for logistic regression are not met.")
  }

  return(list_return)

}


#' Check binary outcome assumption
#'
#' This function checks whether the outcome variable is binary encoded.
#'
#' A key assumption for logistic regression is the outcome is binary encoded,
#' i.e. 1 and 0, 'Yes' and 'No', 'Survived' and 'Died'. If this assumption is
#' not satisfied the Odds Ratio results are affected.
#'
#' NB, `glm` automatically removes any records containing empty outcomes, such
#' as NA or NULL, so this function does not need to check for these.
#'
#' An alert will be printed to the console where this assumption is not upheld.
#' Optionally, additional context can be displayed where `details` = `TRUE`.
#'
#' @param glm Results from a binomial Generalised Linear Model (GLM), as produced by [stats::glm()].
#' @param details Boolean: TRUE = additional details will be printed to the Console if this assumption fails, FALSE = additional details will be suppressed.
#'
#' @returns Boolean: TRUE = assumption is upheld, FALSE = assumption failed
#' @noRd
assumption_binary_outcome <- function(glm, details = FALSE) {

  # data prep ---
  # get the data
  df <- glm$model

  # get the outcome variable name and cast to symbol
  str_outcome <- get_outcome_variable_name(model = glm, return_var_name = TRUE)
  var_outcome <- base::as.symbol(str_outcome)

  # assumption details ---

  # count outcomes as a tibble
  df_outcome_count <- df |> dplyr::count({{var_outcome}})

  # what outcomes are included
  outcome_levels <- df_outcome_count |> dplyr::pull({{var_outcome}})

  # count how many levels are in the outcome
  outcome_level_count <- length(outcome_levels)

  # does the outcome have precisely two levels
  result <- outcome_level_count == 2

  # context details ---

  # what is the data type of the outcome
  outcome_class <- class(outcome_levels)

  # alert details ---

  # alert the user if this assumption is not held
  if (!result) {
    cli::cli_alert_danger(
      "Logistic regression requires a binary outcome variable."
    )
  }

  # provide additional details if requested
  if (!result & details) {
    cli::cli_alert(
      "Your outcome variable {.var {str_outcome}} has {outcome_level_count} level{?s}: {.val {outcome_levels}}.",
      wrap = TRUE
    )
    cli::cli_alert(
      "This alert is commonly caused by errant blank responses such as {.val NA}, {.val Blank} or {.val Unknown}. Check your model data to ensure only two outcome measures are included.",
      wrap = TRUE
    )
  }

  # return the result
  return(result)

}

#' Check for multicollinearity
#'
#' This function checks for correlations between predictor variables and flags
#' cases where this correlation is severe, as indicated by GVIF^(1/(2*Df)) of
#' 5 or more.
#'
#' A key assumption for logistic regression is the predictor variables are
#' independent of each other. Where predictor variables are correlated this
#' can result in:
#'
#' * unstable estimates which are sensitive to small changes in the data,
#' resulting in larger confidence intervals and unstable odds ratios,
#'
#' * inflated variance resulting in larger confidence intervals,
#'
#' * biased coefficients leading to incorrect conclusions about the relationship
#' between predictors and the outcome,
#'
#' * separation issues, in which the model predicts probabilities of 0 or 1.
#'
#' An alert will be printed to the console where this assumption is not upheld.
#' Optionally, additional context can be displayed where `details` = `TRUE`.
#'
#' # Measuring multicollinearity
#'
#' The `vif()` function from the {car} package will be used to work out a
#' variance inflation factor (VIF) for numerical predictors and a measure based
#' on generalised variance inflation factor (GVIF) for models involving a
#' mixture of numerical and categorical predictors.
#'
#' For VIF a threshold of 5 will be used:
#' * below 5: zero to moderate multicollinearity (no alert)
#' * 5 or above: high multicollinearity (alert)
#'
#' Hair, J. F., Black, W. C., Babin, B. J., & Anderson, R. E. (2010). Multivariate data analysis. Prentice Hall.
# Kutner, M. H., Nachtsheim, C. J., & Neter, J. (2005). Applied linear regression models. McGraw-Hill.
#'
#' For GVIF-based measures a threshold of 2 will be used:
#' * below 2: zero to moderate multicollinearity (no alert)
#' * 5 or above: high multicollinearity (alert)
#'
#' Fox, J., & Monette, G. (1992). Generalized collinearity diagnostics. Journal of the American Statistical Association, 87(417), 178-183.
#'
#' Where a predictor is found to have an inflation measure at or above
#' threshold then a warning will be raised alerting the user to the potential
#' presence of correlation. This warning will not prevent the code from
#' executing and producing the desired output.
#'
#' Any warning produced by this function is not prescriptive. The presence of
#' a warning should be a sign to the user to *consider* their model and
#' perform further investigations to satisfy themselves their model is correct.
#'
#' @param glm Results from a binomial Generalised Linear Model (GLM), as produced by [stats::glm()].
#' @param details Boolean: TRUE = additional details will be printed to the Console if this assumption fails, FALSE = additional details will be suppressed.
#'
#' @returns Boolean: TRUE = assumption is upheld, FALSE = assumption failed
#' @noRd
assumption_no_multicollinearity <- function(glm, details = FALSE) {

  # get the variance inflation factor (VIF) or
  df_vif <- car::vif(glm) |>
    tibble::as_tibble(rownames = 'predictor') |>
    janitor::clean_names() |>
    dplyr::rename(
      dplyr::any_of(
        c(
          'vif' = 'value',
          'gvif_scaled' = 'gvif_1_2_df'
        )
      )
    ) |>
    # get the square of any scaled gvif:
    # https://stacyderuiter.github.io/s245-notes-bookdown/collinearity-and-multicollinearity.html
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::any_of('gvif_scaled'),
        .fns = \(.x) .x ^ 2,
        .names = "{.col}_squared"
      )
    )

  # set thresholds for each measure
  var_thresholds <- c('GVIF^(1/(2*Df))^2' = 2, 'VIF' = 5)

  # flag any predictors with above-threshold values
  df_vif <-
    df_vif |>
    dplyr::mutate(
      # models involving continuous predictors only
      dplyr::across(
        .cols = dplyr::any_of('vif'),
        .fns = \(.x) dplyr::case_when(
          .x >= var_thresholds['VIF'] ~ TRUE,
          .default = FALSE
        ),
        .names = "above_threshold"
      ),
      # models involving categorical variables
      dplyr::across(
        .cols = dplyr::any_of('gvif_scaled_squared'),
        .fns = \(.x) dplyr::case_when(
          .x >= var_thresholds['GVIF^(1/(2*Df))^2'] ~ TRUE,
          .default = FALSE
        ),
        .names = "above_threshold"
      )
    )

  # assumption details ---

  # are all predictors within threshold?
  result <- any(df_vif$above_threshold, na.rm = TRUE) == FALSE

  # context details ---

  # what predictors are above threshold
  correlated_predictors <- df_vif$predictor[df_vif$above_threshold == TRUE]

  # how many are there
  correlated_predictor_count <- length(correlated_predictors)

  # what measure of inflation was used
  var_measure <- dplyr::if_else(
    condition =  'gvif_scaled_squared' %in% names(df_vif),
    true = 'GVIF^(1/(2*Df))^2',
    false = 'VIF'
  )

  # what values to they have
  var_values <- df_vif |>
    dplyr::filter(.data$above_threshold) |>
    dplyr::pull(dplyr::any_of(c('vif', 'gvif_scaled_squared'))) |>
    round(digits = 1)

  # alert details ---

  # alert the user if this assumption is not held
  if (!result) {
    cli::cli_warn(
      "Signs of multicollinearity detected in {correlated_predictor_count} of your predictor variables."
    )
  }

  # provide additional details if requested
  if (!result & details) {
    cli::cli_alert(
      "{.var {correlated_predictors}} {?has/have} {var_measure} values of {.val {var_values}}.",
      wrap = TRUE
    )
    cli::cli_alert(
      "{var_measure} values equal to or greater than {.val {var_thresholds[var_measure]}} are indicative of correlation.",
      wrap = TRUE
    )
  }

  # return the result
  return(result)
}
