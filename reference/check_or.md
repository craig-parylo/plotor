# Check OR

Performs a series of tests to ensure that assumptions for logistic
regression are met, with optional detailed feedback if any tests fail.

## Usage

``` r
check_or(glm_model_results, confint_fast_estimate = FALSE, details = TRUE)
```

## Arguments

- glm_model_results:

  Results from a binomial Generalised Linear Model (GLM), as produced by
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html).

- confint_fast_estimate:

  Boolean (default = `FALSE`) Use a faster estimate of the confidence
  interval? Note: this assumes normally distributed data, which may not
  be suitable for your data.

- details:

  Boolean (default = `TRUE`) Show detailed feedback for any failed
  tests?

## Value

Logical, `TRUE` if all assumption tests pass, `FALSE` if one or more
tests fail

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
  family = binomial,
  formula = Survived ~ Class + Sex + Age
)

# Check the model for logistic regression assumption violations
check_or(lr)
#> 
#> ── Assumption checks ───────────────────────────────────────────────────────────
#> 
#> ── Summary ──
#> 
#> ✔ The outcome variable is binary
#> ✔ Predictor variables are not highly correlated with each other
#> ✔ The outcome is not separated by predictors
#> ✔ The sample size is large enough
#> ✔ Continuous variables either have a linear relationship with the log-odds of
#> the outcome or are absent
#> ✔ No observations unduly influence model estimates
#> 
#> Your model was checked for logistic regression assumptions in the following
#> areas:
#> 
#> Binary outcome:
#> The outcome variable was checked for containing precisely two levels.
#> 
#> Multicollinearity:
#> The `vif()` function from the car package was used to check for highly
#> correlated predictor variables.
#> 
#> Separation:
#> The `detectseparation()` function from the detectseparation package was used to
#> check for complete or quasi-complete separation in the data.
#> 
#> Sample size:
#> A rule of thumb was applied, requiring at least 10 events per predictor
#> variable and at least 10 events per level of categorical variables to ensure
#> sufficient data for reliable estimates.
#> 
#> Linearity:
#> A likelihood ratio test was conducted to assess improvements in model fit
#> compared to a model using Box-Tidwell power transformations on continuous
#> predictors. Any observed improvement likely indicates non-linear relationships
#> between the continuous predictors and the log-odds of the outcome.
#> 
#> Influential observations:
#> A test to identify observations that could disproportionately influence model
#> statistics was applied. The test simultaneously examined three metrics: Cook's
#> distance (measuring overall observation impact), leverage (quantifying an
#> observation's distance from the data centre), and standardised residuals
#> (indicating how unusual an observation is relative to the model). To minimise
#> false positive, an observation was flagged only if it met at least two of these
#> diagnostic criteria.
#> 
#> ✔ These tests found no issues with your model.
#> 
```
