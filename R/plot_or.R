# Exported functions -----------------------------------------------------------

#' Plot OR
#'
#' Produces an Odds Ratio plot to visualise the results of a logistic regression
#' analysis.
#'
#' @param glm_model_results Results from a binomial Generalised Linear Model (GLM), as produced by [stats::glm()].
#' @param conf_level Numeric value between 0.001 and 0.999 (default = 0.95) specifying the confidence level for the confidence interval.
#' @param confint_fast_estimate Boolean (default = `FALSE`) indicating whether to use a faster estimate of the confidence interval. Note: this assumes normally distributed data, which may not be suitable for your data.
#' @param assumption_checks Boolean (default = `TRUE`) indicating whether to conduct checks to ensure that the assumptions of logistic regression are met.
#'
#' @return
#' The function returns an object of class `gg` and `ggplot`, which can be
#' customised and extended using various `ggplot2` functions.
#'
#' @seealso
#' * See vignette('using_plotor', package = 'plotor') for more details on usage.
#' * More details and examples can be found on the website: <https://craig-parylo.github.io/plotor/index.html>
#'
#' @export
#' @examples
#' # Load required libraries
#' library(plotor)
#' library(datasets)
#' library(dplyr)
#' library(ggplot2)
#' library(stats)
#' library(forcats)
#' library(tidyr)
#'
#' # Load the Titanic dataset
#' df <- datasets::Titanic |>
#'   as_tibble() |>
#'   # convert aggregated counts to individual observations
#'   filter(n > 0) |>
#'   uncount(weights = n) |>
#'   # convert character variables to factors
#'   mutate(across(where(is.character), as.factor))
#'
#' # Perform logistic regression using `glm`
#' lr <- glm(
#'   data = df,
#'   family = 'binomial',
#'   formula = Survived ~ Class + Sex + Age
#' )
#'
#' # Produce the Odds Ratio plot
#' plot_or(lr)
plot_or <- function(
  glm_model_results,
  conf_level = 0.95,
  confint_fast_estimate = FALSE,
  assumption_checks = TRUE
) {
  # data and input checks ----
  # check the model is logistic regression
  valid_glm_model <- validate_glm_model(glm_model_results)

  # check logistic regression assumptions if the user requested it
  if (assumption_checks) {
    valid_assumptions <- check_assumptions(
      glm = glm_model_results,
      details = FALSE,
      confint_fast_estimate = confint_fast_estimate
    )

    # recommend to the user to use `check_or` for more feedback if at least one
    # test fails
    suggest_check_or(
      # pass in the name of the model
      glm_name = deparse(substitute(glm_model_results)),
      valid_assumptions = valid_assumptions,
      assumption_checks = assumption_checks
    )
  }

  # limit conf_level to between 0.001 and 0.999
  conf_level <- validate_conf_level_input(conf_level)

  # main ----

  # get summary of the data and results
  df <- get_summary_table(
    glm_model_results = glm_model_results,
    conf_level = conf_level,
    confint_fast_estimate = confint_fast_estimate
  )

  # plot the results
  p <- plot_odds_ratio(
    df = df,
    model = glm_model_results,
    conf_level = conf_level
  )

  return(p)
}

#' Table OR
#'
#' @description
#' Produces a formatted table displaying the outputs from the Odds Ratio
#' analysis, including details on covariate characteristics and model results.
#'
#' @details
#' The table includes the following information:
#' - **Covariate Characteristics**:
#'   - Number of observations for each characteristic
#'   - Number of observations resulting in the outcome of interest
#'   - Conversion rate of the outcome based on the number of observations
#'
#' - **Model Results**:
#'   - Estimated Odds Ratio, standard error, and p-value
#'   - Calculated confidence interval for the specified confidence level
#'
#' A visualisation of the Odds Ratio plot is also provided for an at-a-glance
#' view of the model results.
#'
#' If `anonymise_counts` is set to `TRUE`, counts below 10 are suppressed as
#' `<10`, and other counts are rounded to the nearest multiple of 5. This
#' feature is helpful when working with sensitive data.
#'
#' @param glm_model_results Results from a binomial Generalised Linear Model (GLM), as produced by [stats::glm()].
#' @param conf_level Numeric value between 0.001 and 0.999 (default = 0.95) specifying the confidence level for the confidence interval.
#' @param output String describing the output type (default = 'tibble'). Options include 'tibble' and 'gt'.
#' @param confint_fast_estimate Boolean (default = `FALSE`) indicating whether to use a faster estimate of the confidence interval. Note: this assumes normally distributed data, which may not be suitable for your data.
#' @param assumption_checks Boolean (default = `TRUE`) indicating whether to conduct checks to ensure that the assumptions of logistic regression are met.
#' @param anonymise_counts Boolean (default = `FALSE`) indicating whether to anonymise counts in the output table. If `TRUE`, counts less than 10 are suppressed and otherwise rounded to the nearest multiple of 5.
#'
#' @returns
#' The returned object depends on the `output` parameter:
#' - If `output = 'tibble'`, the function returns an object of class "tbl_df", "tbl", and "data.frame".
#' - If `output = 'gt'`, the function returns an object of class "gt_tbl" and "list".
#'
#' @export
#'
#' @examples
#' # Load the Titanic dataset
#' df <- datasets::Titanic |>
#'   dplyr::as_tibble() |>
#'   # convert aggregated counts to individual observations
#'   dplyr::filter(n > 0) |>
#'   tidyr::uncount(weights = n) |>
#'   # convert character variables to factors
#'   dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))
#'
#' # Perform logistic regression using `glm`
#' lr <- stats::glm(
#'   data = df,
#'   family = 'binomial',
#'   formula = Survived ~ Class + Sex + Age
#' )
#'
#' # Produce the Odds Ratio table as a tibble
#' table_or(lr)
#'
#' # Produce the Odds Ratio table as a gt object
#' table_or(lr, output = 'gt')
table_or <- function(
  glm_model_results,
  conf_level = 0.95,
  output = 'tibble',
  confint_fast_estimate = FALSE,
  assumption_checks = TRUE,
  anonymise_counts = FALSE
) {
  # data and input checks ----
  # check the model is logistic regression
  valid_glm_model <- validate_glm_model(glm_model_results)

  # check logistic regression assumptions if the user requested it
  if (assumption_checks) {
    valid_assumptions <- check_assumptions(
      glm = glm_model_results,
      details = FALSE,
      confint_fast_estimate = confint_fast_estimate
    )

    # recommend to the user to use `check_or` for more feedback if at least one
    # test fails
    suggest_check_or(
      # pass in the name of the model
      glm_name = deparse(substitute(glm_model_results)),
      valid_assumptions = valid_assumptions,
      assumption_checks = assumption_checks
    )
  }

  # limit conf_level to between 0.001 and 0.999
  conf_level <- validate_conf_level_input(conf_level)

  # limit output to acceptable types and raise an error if not
  output_valid <- validate_output_table_input(output)

  # main ----
  # get summary of rows and estimate OR
  df <- get_summary_table(
    glm_model_results = glm_model_results,
    conf_level = conf_level,
    confint_fast_estimate = confint_fast_estimate
  )

  # get the outcome variable
  str_outcome <- get_outcome_variable_name(model = glm_model_results)

  # prepare for output
  df <-
    df |>
    # remove variables which aren't necessary for table views
    dplyr::select(
      !dplyr::any_of(c(
        'term',
        'rows_scale',
        'label_or',
        'group',
        'p_label'
      ))
    ) |>
    # work out the rate of 'outcome'
    dplyr::mutate(outcome_rate = .data$outcome / .data$rows) |>
    dplyr::relocate('outcome_rate', .after = 'outcome')

  # anonymise count data if requested
  if (anonymise_counts) {
    df <-
      df |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::any_of(c("outcome", "rows")),
          .fns = ~ anonymise_count_values(.x)
        )
      )
  }

  # decide what object to return
  obj_return <-
    switch(
      output,
      # output a tibble
      'tibble' = {
        df
      },

      # output a gt-formatted table
      'gt' = {
        df |>
          dplyr::group_by(.data$label) |>
          output_gt(conf_level = conf_level, title = str_outcome)
      }
    )

  return(obj_return)
}

#' Check OR
#'
#' Performs a series of tests to ensure that assumptions for logistic regression
#' are met, with optional detailed feedback if any tests fail.
#'
#' @param glm_model_results Results from a binomial Generalised Linear Model (GLM), as produced by [stats::glm()].
#' @param confint_fast_estimate Boolean (default = `FALSE`) Use a faster estimate of the confidence interval? Note: this assumes normally distributed data, which may not be suitable for your data.
#' @param details Boolean (default = `TRUE`) Show detailed feedback for any failed tests?
#'
#' @returns Logical, `TRUE` if all assumption tests pass, `FALSE` if one or more tests fail
#' @export
#'
#' @examples
#' # Load the Titanic dataset
#' df <- datasets::Titanic |>
#'   dplyr::as_tibble() |>
#'   # convert aggregated counts to individual observations
#'   dplyr::filter(n > 0) |>
#'   tidyr::uncount(weights = n) |>
#'   # convert character variables to factors
#'   dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))
#'
#' # Perform logistic regression using `glm`
#' lr <- stats::glm(
#'   data = df,
#'   family = binomial,
#'   formula = Survived ~ Class + Sex + Age
#' )
#'
#' # Check the model for logistic regression assumption violations
#' check_or(lr)
check_or <- function(
  glm_model_results,
  confint_fast_estimate = FALSE,
  details = TRUE
) {
  # set heading
  cli::cli_h1("Assumption checks")

  # get a summary of test results
  # NB, detailed feedback is handled by each of the test functions
  test_results <- check_assumptions(
    glm = glm_model_results,
    confint_fast_estimate = confint_fast_estimate,
    details = details
  )

  # ... detailed feedback appears here ...

  # summarise
  cli::cli_h2("Summary")

  # binary outcome
  if (test_results$assume_binary) {
    cli::cli_alert_success(
      "The outcome variable is binary",
      wrap = TRUE
    )
  } else {
    cli::cli_alert_danger(
      "The outcome variable is not binary",
      wrap = TRUE
    )
  }

  # no multicollinearity
  if (test_results$assume_independent) {
    cli::cli_alert_success(
      "Predictor variables are not highly correlated with each other",
      wrap = TRUE
    )
  } else {
    cli::cli_alert_danger(
      "Predictor variables may be correlated",
      wrap = TRUE
    )
  }

  # no separation
  if (test_results$assume_no_separation) {
    cli::cli_alert_success(
      "The outcome is not separated by predictors",
      wrap = TRUE
    )
  } else {
    cli::cli_alert_danger(
      "The outcome is separated by at least one predictor",
      wrap = TRUE
    )
  }

  # sufficient sample size
  if (test_results$assume_sample_size) {
    cli::cli_alert_success(
      "The sample size is large enough",
      wrap = TRUE
    )
  } else {
    cli::cli_alert_danger(
      "The sample size may not be large enough",
      wrap = TRUE
    )
  }

  # linear relationship with continuous variables and log-odds of outcome
  if (test_results$assume_linearity) {
    cli::cli_alert_success(
      "Continuous variables either have a linear relationship with the log-odds of the outcome or are absent",
      wrap = TRUE
    )
  } else {
    cli::cli_alert_danger(
      "Signs of non-linear relationship detected",
      wrap = TRUE
    )
  }

  # summary text
  cli::cli_par()
  cli::cli_end()
  cli::cli_par()
  cli::cli_text(
    "Your model was checked for logistic regression assumptions in the following areas:"
  )
  cli::cli_end()
  cli::cli_par()
  cli::cli_text("{.emph Binary outcome:}")
  cli::cli_text(
    "The outcome variable was checked for containing precisely two levels."
  )
  cli::cli_end()
  cli::cli_par()
  cli::cli_text("{.emph Multicollinearity:}")
  cli::cli_text(
    "The {.fn vif} function from the {.pkg car} package was used to check for highly correlated predictor variables."
  )
  cli::cli_end()
  cli::cli_par()
  cli::cli_text("{.emph Separation:}")
  if (confint_fast_estimate) {
    cli::cli_text(
      "Numeric predictors were checked for overlapping ranges across both outcomes and categorical predictors were checked for each level appearing at least once in both outcomes."
    )
  } else {
    cli::cli_text(
      "The {.fn detectseparation} function from the {.pkg detectseparation} package was used to check for complete or quasi-complete separation in the data."
    )
  }
  cli::cli_end()
  cli::cli_par()
  cli::cli_text("{.emph Sample size:}")
  cli::cli_text(
    "A rule of thumb was applied, requiring at least 10 events per predictor variable and at least 10 events per level of categorical variables to ensure sufficient data for reliable estimates."
  )
  cli::cli_end()
  cli::cli_par()
  cli::cli_text("{.emph Linearity:}")
  cli::cli_text(
    "A likelihood ratio test was conducted to assess improvements in model fit compared to a model using Box-Tidwell power transformations on continuous predictors. Any observed improvement likely indicates non-linear relationships between the continuous predictors and the log-odds of the outcome."
  )
  cli::cli_end()
  cli::cli_par()
  if (all(unlist(test_results))) {
    cli::cli_alert_success(
      "These tests found no issues with your model.",
      wrap = TRUE
    )
  } else {
    cli::cli_alert_warning(
      "These tests indicate there are issues (reported above) that you may wish to explore further before reporting your findings.",
      wrap = TRUE
    )
  }
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
  outcome_num <- df[[outcome_name]] |>
    stats::na.omit() |>
    as.numeric() |>
    max(na.rm = TRUE)
  outcome_txt <- levels(df[[outcome_name]])[outcome_num]

  var_temp <- df |>
    dplyr::select(tidyselect::all_of(var)) |>
    dplyr::pull()

  # calculate rows - split if categorical
  df <-
    if (is.numeric(var_temp)) {
      df |>
        dplyr::filter(!is.na(var_name)) |>
        dplyr::summarise(
          rows = dplyr::n(),
          outcome = sum({{ outcome }} == outcome_txt)
        ) |>
        dplyr::mutate(group = var_name, level = var_name, term = var_name) |>
        dplyr::select(dplyr::any_of(c(
          'term',
          'group',
          'level',
          'rows',
          'outcome'
        )))
    } else {
      df |>
        dplyr::mutate(
          outcome = sum({{ outcome }} == outcome_txt),
          .by = {{ var }}
        ) |>
        #dplyr::summarise(rows = dplyr::n(), .by = c({{var}}, {{outcome}})) |>
        dplyr::summarise(rows = dplyr::n(), .by = c({{ var }}, 'outcome')) |>
        dplyr::rename(level = {{ var }}) |>
        dplyr::mutate(
          group = var_name,
          term = base::paste0(.data$group, .data$level)
        ) |>
        dplyr::select(dplyr::any_of(c(
          'term',
          'group',
          'level',
          'rows',
          'outcome'
        )))
    }

  # add the class of the variable
  df <-
    df |>
    dplyr::mutate(class = paste(class(var_temp), collapse = " "))

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
  model_data <-
    model_results$model |>
    dplyr::as_tibble()

  # get the model variables
  model_vars = base::all.vars(stats::formula(model_results)[-2])

  # get the outcome variable
  model_outcome = base::all.vars(stats::formula(model_results))[1]

  # get a summary of all model variables and levels (will be used as the spine)
  df <- get_model_variables_and_levels(model_results = model_results)

  # count the number of rows used for each variable and level
  df_rows <-
    model_vars |>
    purrr::map_dfr(\(.x) {
      count_rows_by_variable(
        df = model_data,
        var_name = .x,
        outcome_name = model_outcome
      )
    }) |>
    # rescale rows (will be used to set the size of the dot in the plot)
    dplyr::mutate(
      rows_scale = dplyr::case_when(
        .data$class == 'numeric' ~ 1,
        .default = .data$rows |>
          scales::rescale(to = c(1, 5))
      )
    )

  # combine the two data
  df <-
    df |>
    dplyr::select(dplyr::any_of("term")) |>
    dplyr::left_join(
      y = df_rows,
      by = dplyr::join_by('term' == 'term')
    )

  # return the table summary
  return(df)
}

#' Get a tibble of model variables and levels
#'
#' Returns a tibble containing the variables used in the model and expands it
#' for categorical variables to include details of each level. This includes
#' the reference levels, which is missed by {broom} outputs from the glm model.
#'
#' @param model_results Results from a Generalised Linear Model (GLM) binomial model, as produced by [stats::glm()].
#'
#' @returns Tibble summary of variables and levels used in the model
#' @noRd
get_model_variables_and_levels <- function(model_results) {
  # 1. get a list of all model variables
  model_vars <-
    model_results |>
    stats::formula() |>
    purrr::pluck(3) |>
    base::all.vars() |>
    tibble::enframe() |>
    dplyr::select(
      -dplyr::any_of('name'),
      dplyr::any_of(c('variable' = 'value'))
    )

  # 2. for all categorical variables, list out the levels
  model_var_levels <-
    model_results$xlevels |>
    tibble::enframe() |>
    tidyr::unnest(cols = dplyr::any_of('value')) |>
    dplyr::rename(dplyr::any_of(c(
      'variable' = 1,
      'level' = 2
    )))

  # 3. combine the two sets
  df <-
    model_vars |>
    dplyr::left_join(
      y = model_var_levels,
      by = dplyr::join_by('variable' == 'variable')
    ) |>
    # create the 'term' outputted by {broom}
    dplyr::mutate(term = glue::glue("{variable}{level}", .na = "")) |>
    dplyr::relocate(dplyr::any_of('term'), .before = dplyr::any_of('variable'))

  # return the result
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
    ggplot2::geom_errorbar(
      # remove any confidence estimates with NA values
      data = df |>
        dplyr::filter(!is.na(.data$conf.high), !is.na(.data$conf.low)),
      ggplot2::aes(xmax = .data$conf.high, xmin = .data$conf.low),
      width = 1 / 5,
      na.rm = TRUE
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
      subtitle = glue::glue(
        'Odds Ratio (OR) plot with {str_conf_level} Confidence Interval (CI)'
      ),
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
  dplyr::case_when(
    is.na(dplyr::lag(group)) ~ group,
    group != dplyr::lag(group) ~ group,
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

#' Suggest the Use of check_or()
#'
#' @description
#' This function suggests using the check_or() function for more detailed
#' feedback if any of the assumption tests fail.
#'
#' @details
#' The function outputs a console message that, when any assumption checks
#' fail, recommends that the user run the check_or() function with the details
#' argument set to TRUE. This provides additional insights into the cause of
#' the failed tests.
#'
#' Using the {cli} package, the console message includes a special .run
#' argument to construct a valid function call using the name of the logistic
#' regression model, which is passed in via a parameter. This allows
#' the check_or() function to be executed from the console with a single click.
#'
#' @param glm_name String: The name of the GLM model passed to the package.
#' @param valid_assumptions List: A named list of assumptions and outcomes from the tests of the assumptions, represented as boolean values.
#' @param assumption_checks Boolean: Indicates whether the user requested their model to be tested for logistic regression assumptions.
#'
#' @returns Nothing; this function is used solely to output console feedback to the user.
#' @noRd
suggest_check_or <- function(
  glm_name,
  valid_assumptions,
  assumption_checks
) {
  # have all assumption checks passed?
  all_passed <- all(unlist(valid_assumptions))

  # only progress if at least one test failed and the user requested tests of logistic regression assumptions
  if (!all_passed & assumption_checks) {
    # suggest the user find out more from `check_or()`
    cli::cli_alert_info(
      "One or more assumptions for logistic regression have failed. To gain further insights, consider calling the {.fn plotor::check_or} function. For example, you can run {.run plotor::check_or({glm_name}, details = TRUE)}.",
      wrap = TRUE
    )
  }
}

#' Anonymise Count Values
#'
#' @description
#' Anonymises counts within a supplied numeric vector by applying specific formatting rules.
#'
#' @details
#' This function provides a vectorised approach to data anonymisation that:
#' - Suppresses counts below 10 by replacing them with the string "<10".
#' - Rounds counts of 10 or more to the nearest multiple of 5.
#' - Formats counts using `scales::label_number_auto()` to include thousands separators, which respects the locale settings for choice of the big mark.
#'
#' The function returns a vector of strings representing the anonymised counts.
#'
#' @param var A numeric vector containing count data.
#'
#' @return A character vector of anonymised counts.
anonymise_count_values <- function(var) {
  # Ensure `var` is a numeric vector
  var <- as.numeric(var)

  # Suppress counts below 10 and round the rest to the nearest multiple of 5
  vec_return <- dplyr::case_when(
    var < 10 ~ "<10",
    TRUE ~ scales::label_number_auto()(round(var / 5) * 5) # Round to nearest multiple of 5 and format the number
  )

  # Return the result
  return(vec_return)
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
    conf_level < ci_min | conf_level > ci_max ~
      min(ci_max, max(ci_min, conf_level)),

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
#' @param confint_fast_estimate Boolean (default = `FALSE`) indicating whether to use a faster estimate of the confidence interval. Note: this assumes normally distributed data, which may not be suitable for your data.
#'
#' @returns Tibble providing a summary of the logistic regression model.
#' @noRd
get_summary_table <- function(
  glm_model_results,
  conf_level = 0.95,
  confint_fast_estimate = FALSE
) {
  # get the data from the model object
  df <- summarise_rows_per_variable_in_model(model_results = glm_model_results)

  # get odds ratio and confidence intervals
  if (confint_fast_estimate == TRUE) {
    # use a fast approximation for confidence intervals
    model_or <-
      glm_model_results |>
      # use broom to get or estimates but WITHOUT confidence intervals
      broom::tidy(exponentiate = T, conf.int = F) |>
      # add in confidence interval approximation using `stats::confint.default()`
      dplyr::left_join(
        y = glm_model_results |>
          stats::confint.default(level = conf_level) |>
          exp() |>
          tibble::as_tibble(rownames = "term") |>
          dplyr::rename("conf.low" = 2, "conf.high" = 3),
        by = dplyr::join_by("term" == "term")
      )
  } else {
    # use the correct method to estimate the confidence interval
    model_or <- glm_model_results |>
      broom::tidy(exponentiate = T, conf.int = T, conf.level = conf_level)
  }

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
      columns = c(
        .data$estimate,
        .data$std.error,
        .data$conf.low,
        .data$conf.high
      ),
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
      columns = c(
        .data$estimate,
        .data$std.error,
        .data$statistic,
        .data$p.value
      ),
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
    gt::cols_hide(
      columns = c(
        .data$comparator,
        .data$statistic,
        .data$plot_ci_l,
        .data$plot_ci_u
      )
    ) |>
    # add titles
    gt::tab_header(
      title = gt::md(glue::glue("{title}")),
      subtitle = gt::md(glue::glue(
        "Odds Ratio summary table with {conf_level * 100}% Confidence Interval"
      ))
    ) |>
    # add footnotes
    gt::tab_footnote(
      locations = gt::cells_column_spanners('Characteristic'),
      footnote = gt::md(
        "**Characteristics** are the explanatory variables in the logistic regression analysis. For categorical variables the first characteristic is designated as a reference against which the others are compared. For numeric variables the results indicate a change per single unit increase.\n\n
*Level* - the name or the description of the explanatory variable.\n\n
*N* - the number of observations examined.\n\n
*n* - the number of observations resulting in the outcome of interest.\n\n
*Rate* - the proportion of observations resulting in the outcome of interest (n / N).\n\n
*Class* - description of the data type."
      )
    ) |>
    gt::tab_footnote(
      locations = gt::cells_column_spanners('or'),
      footnote = gt::md(
        "**Odds Ratios** estimate the relative *odds* of an outcome with reference to the *Characteristic*. For categorical data the first level is the reference against which the odds of other levels are compared. Numerical characteristics indicate the change in *OR* for each additional increase of one unit in the variable.\n\n
*OR* - The Odds Ratio point estimate - values below 1 indicate an inverse relationship whereas values above 1 indicate a positive relationship. Values shown to 4 significant figures.\n\n
*SE* - Standard Error of the point estimate. Values shown to 4 significant figures.\n\n
*p* - The p-value estimate based on the residual Chi-squared statistic."
      )
    ) |>
    gt::tab_footnote(
      locations = gt::cells_column_spanners('ci'),
      footnote = gt::md(glue::glue(
        "**Confidence Interval** - the range of values likely to contain the *OR* in {conf_level * 100}% of cases if this study were to be repeated multiple times. If the *CI* touches or crosses the value 1 then it is unlikely the *Characteristic* is significantly associated with the outcome.\n\n
*Lower* & *Upper* - The range of values comprising the *CI*, shown to 4 significant figures.\n\n
*Significance* - The statistical significance indicated by the *CI*, *Significant* where the *CI* does not touch or cross the value 1.
      "
      ))
    ) |>
    # add an OR plot to visualise the results
    # gtExtras::gt_plt_conf_int(
    #   column = 'plot_or',
    #   ci_columns = c('plot_ci_l', 'plot_ci_u'),
    #   ref_line = 0,
    #   text_size = 0
    # ) |>
    gt_plt_conf_int_new(
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
    dplyr::coalesce(
      model_outcome_label,
      model_outcome_var |> base::as.character()
    )

  # return the variable name if requested
  if (return_var_name) {
    return(model_outcome_var)
  } else {
    return(model_outcome)
  }
}


#' Get a Summary Table of Univariable Analysis
#'
#' @description
#' Performs univariable logistic regression for each predictor in a
#' multivariable model, generating a summary table with odds ratios, confidence
#' intervals and p-values.
#'
#' @details
#' This function systematically:
#' - Extracts predictors from a multivariable logistic regression model
#' - Runs individual logistic regression models for each predictor
#' - Calculates odds ratios, confidence intervals and statistical significance
#'
#' Key features:
#' - Flexible confidence interval estimation
#' - Option to use full or model-specific dataset
#' - Comprehensive output for comparative analysis
#'
#' @param glm A binomial Generalised Linear Model (GLM) object from [stats::glm()].
#' @param conf_level Numeric value between 0.001 and 0.999 (default = 0.95) specifying the confidence level for the confidence interval.
#' @param confint_fast_estimate Boolean (default = `FALSE`) indicating whether to use a faster estimate of the confidence interval. Note: this assumes normally distributed data, which may not be suitable for your data.
#' @param use_model_data_only Boolean (default = `FALSE`) indicating whether to use only the subset of data that was used as part of the multivariable model, or set to `TRUE` to use the full set of data provided to the multivariable model. Note, any records containing missing values for any of the outcome or predictor variables is automatically excluded from the multivariable model by {stats::glm}, so the overall number of records used in multivariable models can be much lower than the total number of records supplied to the function. Set to `TRUE` to increase comparability between the univariable and multivariable models, set to `FALSE` to gain a more holistic view of the invididual relationships between predictors and outcome.
#'
#' @returns Tibble providing a summary of the univariable logistic regression model.
#'
#' @examples
#' # prepare the data
#' df <-
#'   # get the dataset
#'   datasets::Titanic |>
#'   # convert to a tibble
#'   tibble::as_tibble() |>
#'   # convert the aggregate counts to individual observations
#'   tidyr::uncount(weights = n) |>
#'   # convert categorical variables to factors
#'   dplyr::mutate_if(
#'     .predicate = is.character,
#'     .funs = as.factor
#'   )
#'
#' # create a model from the data
#' model <-
#'   stats::glm(
#'     formula = Survived ~ Age + Class + Sex,
#'     family = "binomial",
#'     data = df
#'   )
#'
#' # get a univariable summary from all the data
#' plotor:::get_univariable_summary_table(
#'   glm =  model,
#'   confint_fast_estimate = TRUE
#' )
#' @noRd
get_univariable_summary_table <- function(
  glm,
  conf_level = 0.95,
  confint_fast_estimate = FALSE,
  use_model_data_only = FALSE
) {
  # prepare the univariable data
  if (use_model_data_only) {
    # only use data that went into the model
    var_data <- glm$model
  } else {
    # use all data given as part of the multivariable model
    var_data <- glm$data
  }

  # get the outcome variable
  var_outcome <- get_outcome_variable_name(model = glm, return_var_name = TRUE)

  # get a list of the multivariable predictor variables
  var_predictors <-
    get_model_variables_and_levels(model_results = glm) |>
    dplyr::pull(dplyr::any_of("variable")) |>
    unique()

  # iterate over each predictor and perform univariable analysis
  df_return <-
    purrr::map_dfr(
      .x = var_predictors,
      .f = \(.predictor) {
        # get a formula
        uni_formula <- stats::as.formula(
          glue::glue("{var_outcome} ~ {.predictor}")
        )

        # get a model
        uni_glm <-
          stats::glm(
            formula = uni_formula,
            family = "binomial",
            data = var_data
          )

        # summarise the model
        df_summary <- get_summary_table(
          glm_model_results = uni_glm,
          conf_level = conf_level,
          confint_fast_estimate = confint_fast_estimate
        )

        # return the result for collation by {purrr}
        return(df_summary)
      }
    )
  # return the result
  return(df_return)
}


#' Get combined univariable and multivariable summaries
#'
#' @description
#' Produces a combined summary of univariable and multivariable summaries.
#'
#' @details
#' The function starts by getting separate univariable and multivariable summary tables. A combined view is created from variables common to both summaries and the remaining variables prefixed with either 'uv_' for univariable variables or 'mv_' for multivariable variables.
#' These prefixed variables are then left-joined to the combined view to give a single tibble containing details for univariable and multivariable summaries.
#'
#' @param model A binomial Generalised Linear Model (GLM) object from [stats::glm()].
#' @param conf_level Numeric value between 0.001 and 0.999 (default = 0.95) specifying the confidence level for the confidence interval.
#' @param confint_fast_estimate Boolean (default = `FALSE`) indicating whether to use a faster estimate of the confidence interval. Note: this assumes normally distributed data, which may not be suitable for your data.
#' @param use_model_data_only Boolean (default = `FALSE`) indicating whether to use only the subset of data that was used as part of the multivariable model, or set to `TRUE` to use the full set of data provided to the multivariable model. Note, any records containing missing values for any of the outcome or predictor variables is automatically excluded from the multivariable model by {stats::glm}, so the overall number of records used in multivariable models can be much lower than the total number of records supplied to the function. Set to `TRUE` to increase comparability between the univariable and multivariable models, set to `FALSE` to gain a more holistic view of the invididual relationships between predictors and outcome.
#'
#' @returns Tibble providing a combined summary table of univariable and multivariable summaries
#' @noRd
get_combined_summaries <- function(
  model,
  conf_level = 0.95,
  confint_fast_estimate = FALSE,
  use_model_data_only = TRUE
) {
  # gather some data ----
  # define a list of variables common to both datasets, which varies depending
  # on whether the univariable summary is based on model data only
  if (use_model_data_only) {
    common_vars <- c("group", "label", "level", "class", "rows", "outcome")
  } else {
    common_vars <- c("group", "label", "level", "class")
  }

  # get a multivariable summary
  mv_summary <-
    get_summary_table(
      glm_model_results = model,
      conf_level = conf_level,
      confint_fast_estimate = confint_fast_estimate
    )

  # get a univariable summary
  uv_summary <-
    get_univariable_summary_table(
      glm = model,
      conf_level = conf_level,
      confint_fast_estimate = confint_fast_estimate,
      use_model_data_only = use_model_data_only
    )

  # main ----
  # start the table with the common variables plus 'term' as the common field for subsequent table joins
  combined_summary <-
    mv_summary |>
    dplyr::select(c("term", dplyr::any_of(common_vars)))

  # combine uv and mv summaries to a list
  summary_list <- list("uv" = uv_summary, "mv" = mv_summary)

  # iterate over each summary and prefix variable names with either 'uv_'
  # or 'mv_', depending on whether the summary is univariable or multivariable
  summary_list <-
    purrr::imap(
      .x = summary_list,
      .f = \(.summary, .name) {
        # process the tibble
        df <-
          .summary |>
          # exclude the common variables
          dplyr::select(!dplyr::any_of(common_vars)) |>
          # prefix variables with the name of the list item,
          # (e.g. 'uv' or 'mv') except 'term'
          dplyr::rename_with(
            .cols = !dplyr::all_of("term"),
            .fn = ~ glue::glue("{.name}_{.x}")
          )
      }
    )

  # left-join these summaries to the combined summary
  combined_summary <-
    combined_summary |>
    dplyr::left_join(
      y = summary_list[[1]],
      by = dplyr::join_by("term" == "term")
    ) |>
    dplyr::left_join(
      y = summary_list[[2]],
      by = dplyr::join_by("term" == "term")
    )

  # return the result
  return(combined_summary)
}

## assumptions funcs -----------------------------------------------------------

#' Check assumptions
#'
#' Checks whether the supplied `glm` model satisfies assumptions of a binary
#' logistic regression model.
#'
#' The assumptions tested are:
#' * the outcome variable is binary encoded,
#' * there is no multicollinearity among the predictor variables,
#' * the outcome variable is not separated by any of the predictor variables,
#' * the sample size is sufficient to avoid biased estimates
#'
#' @param glm Results from a binomial Generalised Linear Model (GLM), as produced by [stats::glm()].
#' @param details Boolean: TRUE = additional details will be printed to the Console if this assumption fails, FALSE = additional details will be suppressed.
#'
#' @returns Named list indicating the results of each assumption
#' @noRd
check_assumptions <- function(
  glm,
  details = FALSE,
  confint_fast_estimate = FALSE
) {
  # check assumptions
  list_return <- list(
    assume_binary = assumption_binary_outcome(glm = glm, details = details),

    assume_independent = assumption_no_multicollinearity(
      glm = glm,
      details = details
    ),

    assume_no_separation = ifelse(
      test = confint_fast_estimate,
      yes = assumption_no_separation_fast(glm = glm, details = details),
      no = assumption_no_separation(glm = glm, details = details)
    ),

    assume_sample_size = assumption_sample_size(glm = glm, details = details),

    assume_linearity = assumption_linearity(glm = glm, details = details)
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
  df_outcome_count <- df |> dplyr::count({{ var_outcome }})

  # what outcomes are included
  outcome_levels <- df_outcome_count |> dplyr::pull({{ var_outcome }})

  # count how many levels are in the outcome
  outcome_level_count <- length(outcome_levels)

  # does the outcome have precisely two levels
  result <- outcome_level_count == 2

  # context details ---

  # what is the data type of the outcome
  outcome_class <- class(outcome_levels)

  # alert details ---

  # abort processing if this assumption is not held
  if (!result) {
    cli::cli_abort(
      "Logistic regression requires a binary outcome variable."
    )
  }

  # provide additional details if requested
  if (!result & details) {
    cli::cli_h1("Binary outcome assumption")
    cli::cli_alert(
      "Your outcome variable {.var {str_outcome}} has {outcome_level_count} level{?s}: {.val {outcome_levels}}.",
      wrap = TRUE
    )
    cli::cli_alert(
      "This alert is commonly caused by errant blank responses such as {.val NA}, {.val Blank} or {.val Unknown}. Check your model data to ensure only two outcome measures are included.",
      wrap = TRUE
    )

    # provide general advice on this assumption
    cli::cli_h3("About")
    cli::cli_alert_info(
      "The binary outcome assumption in logistic regression is essential because the model predicts probabilities for two categories (e.g., success/failure). This allows the logistic function to map outputs to values between 0 and 1, ensuring interpretability. Without a binary outcome, the model's log-odds transformation would be invalid, making it unsuitable for non-binary classifications.",
      wrap = TRUE
    )
    cli::cli_alert_info(
      "Your data was checked to ensure that your outcome variable contains exactly {.val two} levels, either as a factor or as a binary number of length {.val 2}.",
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
#' * 2 or above: high multicollinearity (alert)
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
        .fns = \(.x) .x^2,
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
        .fns = \(.x) {
          dplyr::case_when(
            .x >= var_thresholds['VIF'] ~ TRUE,
            .default = FALSE
          )
        },
        .names = "above_threshold"
      ),
      # models involving categorical variables
      dplyr::across(
        .cols = dplyr::any_of('gvif_scaled_squared'),
        .fns = \(.x) {
          dplyr::case_when(
            .x >= var_thresholds['GVIF^(1/(2*Df))^2'] ~ TRUE,
            .default = FALSE
          )
        },
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
    condition = 'gvif_scaled_squared' %in% names(df_vif),
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
    cli::cli_h1("No multicollinearity assumption")
    cli::cli_alert_warning(
      "Signs of multicollinearity detected in {correlated_predictor_count} of your predictor variables.",
      wrap = TRUE
    )
    cli::cli_alert(
      "{.var {correlated_predictors}} {?has/have} {var_measure} values of {.val {var_values}}.",
      wrap = TRUE
    )
    cli::cli_alert(
      "{var_measure} values equal to or greater than {.val {var_thresholds[var_measure]}} are indicative of correlation.",
      wrap = TRUE
    )

    # provide general advice on this assumption
    cli::cli_h3("About")
    cli::cli_alert_info(
      "The assumption of no multicollinearity in logistic regression is important because it ensures that the independent variables are not highly correlated. High multicollinearity can inflate standard errors, making it difficult to determine the individual effect of each predictor on the outcome. This can lead to unreliable coefficient estimates and hinder the model's interpretability and predictive power.",
      wrap = TRUE
    )
    cli::cli_alert_info(
      "Your data was analysed using the {.fn vif} function from the {.pkg car} package to calculate the variance inflation factor (VIF) for numerical predictors, and a generalised variance inflation factor (GVIF) for models that include both numerical and categorical predictors.",
      wrap = TRUE
    )
    cli::cli_ul()
    cli::cli_li(
      "For the VIF, a threshold of 5 or higher indicates multicollinearity."
    )
    cli::cli_li(
      "For the GVIF-based measures, a threshold of 2 or higher is used to indicate multicollinearity"
    )
  }

  # return the result
  return(result)
}

#' Check for separation
#'
#' This function checks for potential issues with separation.
#'
#' Complete or quasi-complete separation in logistic regression occurs when a
#' predictor variable (or a combination of predictor variables) perfectly or
#' almost perfectly predicts the outcome variable. This can lead to unstable
#' estimates of the regression coefficients and standard errors, causing
#' problems in model interpretation and inference.
#'
#' This function uses the {detectseparation} package to identify models with
#' infinite maximum likelihood estimates.
#'
#' Where a predictor is found to have a potential issue with separation then a
#' warning will be raised alerting the user. This warning will not prevent the
#' code from executing and producing the desired output.
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
assumption_no_separation <- function(glm, details = FALSE) {
  # get the model data
  glm_df <- glm$model

  # get the model formula
  glm_fm <- glm$formula

  # run the detect separation
  glm_ds <- stats::glm(
    data = glm_df,
    formula = glm_fm,
    family = "binomial",
    method = detectseparation::detect_separation
  )

  # extract the outcome (TRUE = separation detected, otherwise FALSE)
  separation <- glm_ds$outcome

  # negate this to reflect whether assumption is upheld
  result <- !separation

  # identify which predictor variables are responsible for separation
  if (separation) {
    # get the predictor variables
    var_predictors <- attr(glm$terms, "term")

    # iterate over these and re-run to determine which affect separation result
    var_separation <-
      purrr::map_dfr(
        .x = var_predictors,
        .f = function(.predictor) {
          # remove the predictor from the formula
          glm_fm_test <- stats::update(glm_fm, paste("~ . -", .predictor))

          # re-run the fit
          glm_ds_test <- stats::update(object = glm_ds, formula. = glm_fm_test)

          # return the result
          tibble::tibble(
            predictor = .predictor,
            separation = glm_ds_test$outcome != glm_ds$outcome
          )
        }
      )

    # list the variables which are affecting separation result
    var_separation_sig <-
      var_separation |>
      dplyr::filter(separation) |>
      dplyr::pull("predictor")
  }

  # alert details ---

  # alert the user if this assumption is not held
  if (!result) {
    cli::cli_warn(
      "Signs of separation detected in {length(var_separation_sig)} of your predictor variables."
    )
  }

  # provide additional details if requested
  if (!result & details) {
    cli::cli_h1("No separation assumption")
    cli::cli_alert_warning(
      "Signs of separation detected in {length(var_separation_sig)} of your predictor variables."
    )
    cli::cli_alert(
      "{.var {var_separation_sig}} {?is/are} associated with either complete or quasi-complete separation.",
      wrap = TRUE
    )
    cli::cli_alert("The Odds Ratio estimates are likely to be unreliable.")

    # provide general advice on this assumption
    cli::cli_h3("About")
    cli::cli_alert_info(
      "The assumption of no separation in logistic regression is important because it ensures that the predictor variables do not perfectly predict the outcome variable. If separation occurs, it can lead to infinite estimates for the coefficients, making the model unstable and unreliable. This can result in difficulties in interpretation and hinder the model's ability to generalise to new data.",
      wrap = TRUE
    )
    cli::cli_alert_info(
      "Your data was analysed using the {.fn detectseparation} function from the {.pkg detectseparation} package to identify models with infinite maximum likelihood estimates and determine which predictor variable(s) are responsible.",
      wrap = TRUE
    )
  }

  # return the result
  return(result)
}

#' Check for complete separation in logistic regression
#'
#' This function checks for potential issues with complete separation.
#'
#' Complete separation happens when one or more predictors perfectly predict
#' the binary outcome. In such cases, coefficient estimates and standard
#' errors can become unstable, undermining inference and interpretation.
#'
#' This function tests each predictor in a binomial GLM for potential
#' separation:
#' - Numeric predictors: checks whether the range of values for each
#'   outcome class overlaps. Non-overlapping ranges signal separation.
#' - Categorical predictors: checks whether any factor level has zero
#'   observations in one of the outcome classes.
#'
#' If a predictor shows signs of separation, a warning is issued. The warning
#' does not stop execution but indicates you should review your model and data
#' more closely.
#'
#' @param glm Results from a binomial Generalised Linear Model (GLM), as produced by [stats::glm()].
#' @param details Boolean: TRUE = additional details will be printed to the Console if this assumption fails, FALSE = additional details will be suppressed.
#'
#' @returns Boolean: TRUE = assumption is upheld, FALSE = assumption failed
#' @noRd
assumption_no_separation_fast <- function(glm, details = FALSE) {
  # gather some information
  df <- glm$model
  outcome <- glm$formula[[2]]
  predictors <-
    summarise_rows_per_variable_in_model(glm) |>
    dplyr::select(dplyr::all_of(c("group", "class"))) |>
    dplyr::distinct()

  # test each predictor for separation
  results <-
    purrr::map2_dfr(
      .x = predictors$group,
      .y = predictors$class %in% c('integer', 'numeric'),
      .f = \(.pred, .numeric) {
        # test for separation depends on whether the predictor is numeric
        if (.numeric) {
          # work out the predictor ranges for each outcome
          range <-
            df |>
            dplyr::summarise(
              low = min({{ .pred }}, na.rm = TRUE),
              high = max({{ .pred }}, na.rm = TRUE),
              .by = outcome
            )

          # do the ranges overlap?
          result <-
            (range[[1, 2]] <= range[[2, 3]]) &
            (range[[2, 2]] <= range[[1, 3]])
        } else {
          # a factor variable:
          # do any levels result in zero outcomes?
          result <-
            df |>
            # count the outcomes by the predictor
            dplyr::count({{ outcome }}, {{ .pred }}) |>
            # put the outcome as columns
            tidyr::pivot_wider(
              names_from = {{ outcome }},
              values_from = "n",
              values_fill = 0
            ) |>
            dplyr::rename("n0" = 2, "n1" = 3) |>
            dplyr::filter("n0" == 0 | "n1" == 0) |>
            dplyr::summarise(separated = dplyr::n() > 0) |>
            dplyr::pull("separated")
        }

        # return the result
        df_result <- tibble::tibble(
          predictor = .pred,
          separation = result
        )
        return(df_result)
      }
    )

  # consolidate the results to a single TRUE / FALSE
  result <- !results$separation |> any(na.rm = TRUE)

  # list predictors where there are signs of separation
  var_separation <- results |>
    dplyr::filter("separation" == TRUE) |>
    dplyr::pull("predictor")

  # alert details ---

  # alert the user if this assumption is not held
  if (!result) {
    cli::cli_warn(
      "Signs of separation detected in {length(var_separation)} of your predictor variables."
    )
  }

  # provide additional details if requested
  if (!result && details) {
    cli::cli_h1("No separation assumption")
    cli::cli_alert_warning(
      "Signs of separation detected in {length(var_separation)} of your predictor variables."
    )
    cli::cli_alert(
      "{.var {var_separation}} {?is/are} associated with complete separation.",
      wrap = TRUE
    )
    cli::cli_alert("The Odds Ratio estimates are likely to be unreliable.")

    # provide general advice on this assumption
    cli::cli_h3("About")
    cli::cli_alert_info(
      "The assumption of no separation in logistic regression is important because it ensures that the predictor variables do not perfectly predict the outcome variable. If separation occurs, it can lead to infinite estimates for the coefficients, making the model unstable and unreliable. This can result in difficulties in interpretation and hinder the model's ability to generalise to new data.",
      wrap = TRUE
    )
    cli::cli_alert_info(
      "Your data was analysed for complete separation. For numeric predictors, the ranges of each outcome class were compared. Non-overlapping ranges indicate separation. For categorical predictors, each factor level was checked to ensure it appears in both outcome classes; any level found in only one class signals separation.",
      wrap = TRUE
    )
  }

  # return the results
  return(result)
}

#' Check for minimum sample size
#'
#' This function checks whether the sample size is large enough.
#'
#' Binary logistic regression has an assumed minimum sample size because it is
#' based on maximum likelihood estimation, which requires a sufficient number
#' of observations to provide reliable estimates of the model parameters.
#'
#' The rule of thumb is at least 10 events (least frequent outcome) per
#' predictor.
#'
#' Where the sample size is not large enough it may result in biased estimates,
#' large standard errors, possible over fitting, lack of power and unreliable
#' confidence intervals.
#'
#' Where the sample size is potentially too small then a warning will be raised
#' alerting the user. This warning will not prevent the code from executing and
#' producing the desired output.
#'
#' Any warning produced by this function is not prescriptive. The presence of
#' a warning should be a sign to the user to *consider* their model and
#' perform further investigations to satisfy themselves their model is correct.
#'
#' @param glm Results from a binomial Generalised Linear Model (GLM), as produced by [stats::glm()].
#' @param min_events_per_predictor Integer - minimum number of events per predictor (default = 10)
#' @param details Boolean: TRUE = additional details will be printed to the Console if this assumption fails, FALSE = additional details will be suppressed.
#'
#' @returns Boolean: TRUE = assumption is upheld, FALSE = assumption failed
#' @noRd
assumption_sample_size <- function(
  glm,
  min_events_per_predictor = 10,
  details = FALSE
) {
  # get the model data
  glm_df <- glm$model

  # step 1 - check for minimum number of observations per variable
  # gather some details
  events <- sum(glm$y == 1)
  non_events <- sum(glm$y == 0)
  num_predictors <- length(glm$coefficients) - 1
  event_threshold <- min_events_per_predictor * num_predictors

  # test whether assumption is upheld
  result <-
    (events >= event_threshold) &
    (non_events >= event_threshold)

  # determine if there are too few events or non-events
  if (!result) {
    result_cause <-
      if (events < event_threshold & non_events < event_threshold) {
        "events and non-events"
      } else if (events < event_threshold) {
        "events"
      } else {
        "non-events"
      }
  }

  # step 2 - check for minimum number of observations per level of each
  # categorical predictor

  # set up the flag
  result_factors <- TRUE

  # get the name of the outcome variable
  temp_outcome_var <- glm$terms[[2]]

  # get a vector of predictor variables which are factors
  predictor_factors <-
    # get the class of each term
    sapply(glm$model, class) |>
    # convert to a tibble and name terms as 'predictor'
    tibble::as_tibble(rownames = c("predictor")) |>
    # remove the outcome and keep only predictors formatted as factors
    dplyr::filter(
      .data$value == "factor",
      .data$predictor != temp_outcome_var
    ) |>
    # pull a list of predictors
    dplyr::pull(.data$predictor)

  # only proceed if there is at least one factor predictor
  if (length(predictor_factors) > 0) {
    # count observations by each level of the factor predictors
    predictor_factor_level_count <-
      purrr::map_dfr(
        .x = predictor_factors,
        .f = function(.var = .data$.x, .df = glm$model) {
          # rename the outcome variable and standardise the levels
          .df <-
            .df |>
            dplyr::rename(outcome = dplyr::all_of(temp_outcome_var))

          levels(.df$outcome)[1] <- ".nonevent"
          levels(.df$outcome)[2] <- ".event"

          # count the number of observations in each level of predictor
          df <-
            .df |>
            # count rows by the outcome for each predictor variable (.var) level
            dplyr::summarise(
              predictor = {{ .var }},
              n = dplyr::n(),
              .by = c("outcome", {{ .var }})
            ) |>
            # rename var to level and move predictor to start of tibble
            dplyr::rename(level = {{ .var }}) |>
            dplyr::relocate("predictor", .before = "level") |>
            # sort by count (in case this needs displaying)
            dplyr::arrange(dplyr::desc(.data$n)) |>
            # pivot outcomes to their own columns
            tidyr::pivot_wider(
              names_from = dplyr::any_of("outcome"),
              values_from = "n"
            ) |>
            # replace any NA values with zeroes (in cases of complete separation)
            dplyr::mutate(
              dplyr::across(
                .cols = c(".event", ".nonevent"),
                .fns = ~ dplyr::coalesce(.x, 0L)
              )
            )
        }
      )

    # test the condition
    result_factors <-
      (min(predictor_factor_level_count$.nonevent) >=
        min_events_per_predictor) &
      (min(predictor_factor_level_count$.event) >= min_events_per_predictor)

    # gather some additional information
    predictor_factor_level_too_small <-
      predictor_factor_level_count |>
      dplyr::filter(
        .data$.nonevent < min_events_per_predictor |
          .data$.event < min_events_per_predictor
      )
  }

  # alert details ---

  # set a heading for this feedback
  if ((!result | !result_factors) & details) {
    cli::cli_h1("Sample size assumption")
  }

  # alert the user if this assumption is not held
  if (!result) {
    cli::cli_warn(
      "The sample size may be too small relative to the number of predictor variables.",
      wrap = TRUE
    )
  } else if (!result_factors) {
    cli::cli_warn(
      "Some of your categorical predictor variables have levels with too few outcomes.",
      wrap = TRUE
    )
  }

  # provide additional details if requested
  if (!result & details) {
    cli::cli_alert_warning(
      "Your sample size may be too small relative to the number of predictor variables.",
      wrap = TRUE
    )
    cli::cli_alert(
      "Based on a minimum of {.val {min_events_per_predictor}} events per predictor you need at least {.val {event_threshold}} events / non-events (whichever is smaller) for your {num_predictors} predictor variable{?s}.",
      wrap = TRUE
    )
    cli::cli_alert(
      "Your model contains {.val {events}} event{?s} and {.val {non_events}} non-event{?s}.",
      wrap = TRUE
    )
    cli::cli_alert(
      "There may be too few {result_cause} in your model.",
      wrap = TRUE
    )
  }

  # provide feedback with additional details for factor predictors with too few observations
  if (!result_factors & details) {
    cli::cli_h3("Categorical predictors")
    cli::cli_alert_warning(
      "Too few outcomes per level of categorical predictors.",
      wrap = TRUE
    )
    cli::cli_alert(
      "{nrow(predictor_factor_level_too_small)} predictor variable level{?s} in your model {?has/have} fewer than {.val {min_events_per_predictor}} events and / or non-events:",
      wrap = TRUE
    )
    print(predictor_factor_level_too_small)
  }

  # provide general advice on this assumption
  if ((!result | !result_factors) & details) {
    cli::cli_h3("About")
    cli::cli_alert_info(
      "A minimum sample size is an important assumption for obtaining reliable and valid results in logistic regression.",
      wrap = TRUE
    )
    cli::cli_alert_info(
      "Your data was checked using a 'rule of thumb' for binary logistic regression of at least {.val {min_events_per_predictor}} events per predictor variable, and for at least {.val {min_events_per_predictor}} events for each level of any categorical predictor variable.",
      wrap = TRUE
    )
  }

  # return the result
  return(result & result_factors)
}

#' Check for Linearity in Logistic Regression
#'
#' @description
#' Evaluates the linearity assumption in logistic regression by testing
#' whether continuous predictors have a linear relationship with the log-odds
#' of the outcome.
#'
#' @details
#' Logistic regression assumes that continuous predictors have a linear
#' relationship with the log-odds of the outcome. This allows transforming
#' probabilities into a linear space (log-odds) where predictors can have
#' a linear relationship, while keeping final probabilities between 0 and 1.
#'
#' @section Potential Issues:
#' Violation of the linearity assumption can lead to:
#' - Biased odds ratio estimates
#' - Reduced model accuracy
#' - Misspecification of predictor effects
#'
#' @section Methodology:
#' The function uses the Box-Tidwell power transformation:
#' - Creates interaction terms between continuous predictors and their log
#' - Compares the original model with an expanded model including these interactions
#' - Performs a likelihood ratio test to assess model fit improvement
#'
#' @section Interpretation:
#' - Significant likelihood ratio test suggests non-linear relationships
#' - Significant interaction term p-values indicate specific predictors
#'   that may benefit from transformation
#'
#' @note
#' Warnings are not prescriptive. They suggest further investigation
#' is needed to validate model assumptions.
#' @param glm Results from a binomial Generalised Linear Model (GLM), as produced by `stats::glm()`
#' @param details Boolean: TRUE = additional details will be printed to the Console if this assumption fails, FALSE = additional details will be suppressed.
#' @param p_val_threshold Numeric - what value of p-value is the threshold below which the assumption of linearity is upheld, default = 0.05
#'
#' @returns Boolean: TRUE = assumption is upheld, FALSE = assumption failed
#' @noRd
assumption_linearity <- function(glm, details = FALSE, p_val_threshold = 0.05) {
  # get a summary of predictors used in the model
  predictors <- summarise_rows_per_variable_in_model(glm)

  # get the continuous predictors
  predictors_continuous <-
    predictors |>
    dplyr::filter(class %in% c("numeric", "integer")) |>
    dplyr::pull("group") |>
    unique()

  # conduct the test (if there is at least one continuous predictor)
  if (length(predictors_continuous) == 0) {
    # set the result as 'TRUE', i.e. there are no issues with linearity
    result = TRUE
  } else {
    # create interaction terms between continuous variables and their log
    interaction_terms <-
      purrr::map(
        .x = predictors_continuous,
        # NB, using log1p in case there are any zeroes
        .f = \(.x) glue::glue("I({.x} * log1p({.x}))")
      )

    # convert the list to a vector
    interaction_terms <- unlist(interaction_terms)

    # add the interaction terms to the formula
    expanded_formula <-
      stats::reformulate(
        termlabels = c(all.vars(glm$formula)[-1], interaction_terms),
        response = all.vars(glm$formula)[1]
      )

    # fit the expanded model
    expanded_glm <-
      stats::glm(
        formula = expanded_formula,
        family = "binomial",
        data = glm$model
      )

    # perform the likelihood ratio test
    lr_test <-
      stats::anova(
        glm,
        expanded_glm,
        test = "Chisq"
      ) |>
      janitor::clean_names()

    # extract the p-values for the interaction terms
    interaction_pvalues <-
      summary(expanded_glm)$coefficients |>
      tibble::as_tibble(rownames = "term") |>
      dplyr::filter(.data$term %in% interaction_terms) |>
      janitor::clean_names()

    # summarise the results

    # get the p-value from the likelihood ratio test
    linearity_probability <-
      lr_test |>
      tibble::as_tibble() |>
      dplyr::slice_tail(n = 1) |>
      dplyr::pull("pr_chi")

    # is the assumption upheld?
    result <- linearity_probability > p_val_threshold

    # which predictors are not linearly related?
    predictors_nonlinear <-
      interaction_pvalues |>
      # select those who are unlikely to be linear
      dplyr::filter(.data$pr_z <= p_val_threshold) |>
      dplyr::mutate(
        # extract the original term from the interaction term
        term_original = .data$term |>
          stringr::str_extract("(?<=I\\()([^*]+)(?=\\s*\\*)") |>
          stringr::str_trim()
      ) |>
      # pull out to a vector
      dplyr::pull("term_original")
  }

  # alert details ---

  # alert the user if this assumption is not held
  if (!result) {
    cli::cli_warn(
      "Signs of a non-linear relationship detected in {length(predictors_nonlinear)} of your continuous predictor variables.",
      wrap = TRUE
    )
  }

  # provide additional details if requested
  if (!result & details) {
    cli::cli_h1("Linearity assumption")
    cli::cli_alert_warning(
      "Signs of a non-linear relationship detected in {length(predictors_nonlinear)} of your continuous predictor variables.",
      wrap = TRUE
    )
    cli::cli_alert(
      "{.var {predictors_nonlinear}} {?appears/appear} to have non-linear relationship with the log-odds of the outcome.",
      wrap = TRUE
    )
    cli::cli_alert("The Odds Ratio estimates are likely to be unreliable.")

    # provide general advice on this assumption
    cli::cli_h3("About")
    cli::cli_alert_info(
      "When the linearity assumption holds, each unit change in a continuous predictor produces a consistent, predictable change in the log-odds of the outcome. This consistency is crucial for reliable statistical inference. However, when linearity is violated, the model can produce misleading Odds Ratio estimates.",
      wrap = TRUE
    )
    cli::cli_alert_info(
      "Your data was analysed using a Box-Tidwell power transformation with interaction terms between continuous predictors and their log. A likelihood ratio test found the transformed model a better fit to the data, indicating there are non-linear relationships in your model data",
      wrap = TRUE
    )
  }

  # return the result
  return(result)
}
