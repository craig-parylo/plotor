# Table OR

Produces a formatted table displaying the outputs from the Odds Ratio
analysis, including details on covariate characteristics and model
results.

## Usage

``` r
table_or(
  glm_model_results,
  conf_level = 0.95,
  output = c("tibble", "gt"),
  output_type = c("multivariable", "uni_and_multi"),
  confint_fast_estimate = FALSE,
  assumption_checks = TRUE,
  anonymise_counts = FALSE,
  use_model_data_only = TRUE
)
```

## Arguments

- glm_model_results:

  Results from a binomial Generalised Linear Model (GLM), as produced by
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html).

- conf_level:

  Numeric value between 0.001 and 0.999 (default = 0.95) specifying the
  confidence level for the confidence interval.

- output:

  String describing the output type (default = "tibble"). Options
  include "tibble" and "gt".

- output_type:

  String description of the output type (default = "multivariable").
  Options include "multivariable" and "uni_and_multi". Selecting
  "multivariable" will produce a summary table of the supplied
  multivariable model. Selecting "uni_and_multi" will produce a summary
  table showing estimates of the Odds Ratio, Confidence Intervals and
  p-values produced using a univariable logistic regression model for
  each predctor along with the adjusted Odds Ratio, Confidence Intervals
  and p-values from the supplied multivariable model.

- confint_fast_estimate:

  Boolean (default = `FALSE`) indicating whether to use a faster
  estimate of the confidence interval. Note: this assumes normally
  distributed data, which may not be suitable for your data.

- assumption_checks:

  Boolean (default = `TRUE`) indicating whether to conduct checks to
  ensure that the assumptions of logistic regression are met.

- anonymise_counts:

  Boolean (default = `FALSE`) indicating whether to anonymise counts in
  the output table. If `TRUE`, counts less than 10 are suppressed and
  otherwise rounded to the nearest multiple of 5.

- use_model_data_only:

  Boolean (default = `FALSE`) indicating whether to use only the subset
  of data that was used as part of the multivariable model, or set to
  `TRUE` to use the full set of data provided to the multivariable
  model. Note, any records containing missing values for any of the
  outcome or predictor variables are automatically excluded from the
  multivariable model by
  [`stats::glm`](https://rdrr.io/r/stats/glm.html), so the overall
  number of records used in multivariable models can be much lower than
  the total number of records supplied to the function. Set to `TRUE` to
  increase comparability between the univariable and multivariable
  models, set to `FALSE` to gain a more holistic view of the invididual
  relationships between predictors and outcome.

## Value

The returned object depends on the `output` parameter:

- If `output = 'tibble'`, the function returns an object of class
  "tbl_df", "tbl", and "data.frame".

- If `output = 'gt'`, the function returns an object of class "gt_tbl"
  and "list".

## Details

The table includes the following information:

- **Covariate Characteristics**:

  - Number of observations for each characteristic

  - Number of observations resulting in the outcome of interest

  - Conversion rate of the outcome based on the number of observations

- **Model Results**:

  - Estimated Odds Ratio, standard error, and p-value

  - Calculated confidence interval for the specified confidence level

A visualisation of the Odds Ratio plot is also provided for an
at-a-glance view of the model results.

If `anonymise_counts` is set to `TRUE`, counts below 10 are suppressed
as `<10`, and other counts are rounded to the nearest multiple of 5.
This feature is helpful when working with sensitive data.

## Examples

``` r
# Load the Titanic dataset
df <- datasets::Titanic |>
  dplyr::as_tibble() |>
  # convert aggregated counts to individual observations
  dplyr::filter(n > 0) |>
  tidyr::uncount(weights = n) |>
  # convert character variables to factors
  dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))

# Perform logistic regression using `glm`
lr <- stats::glm(
  data = df,
  family = 'binomial',
  formula = Survived ~ Class + Sex + Age
)

# Produce the Odds Ratio table as a tibble
table_or(lr)
#> # A tibble: 8 × 14
#>   label level   rows outcome outcome_rate class  estimate std.error statistic
#>   <fct> <fct>  <int>   <int>        <dbl> <chr>     <dbl>     <dbl>     <dbl>
#> 1 Class 1st      325     203        0.625 factor  NA         NA         NA   
#> 2 Class 2nd      285     118        0.414 factor   0.361      0.196     -5.19
#> 3 Class 3rd      706     178        0.252 factor   0.169      0.172    -10.4 
#> 4 Class Crew     885     212        0.240 factor   0.424      0.157     -5.45
#> 5 Sex   Female   470     344        0.732 factor  NA         NA         NA   
#> 6 Sex   Male    1731     367        0.212 factor   0.0889     0.140    -17.2 
#> 7 Age   Adult   2092     654        0.313 factor  NA         NA         NA   
#> 8 Age   Child    109      57        0.523 factor   2.89       0.244      4.35
#> # ℹ 5 more variables: p.value <dbl>, conf.low <dbl>, conf.high <dbl>,
#> #   significance <chr>, comparator <dbl>

# Produce the Odds Ratio table as a gt object
table_or(lr, output = 'gt')


  


Survived
```
