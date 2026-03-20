# check_or - Quick diagnostics for logistic regression

``` r
library(plotor)
set.seed(123) # reproducibility
```

## Overview

[`check_or()`](https://craig-parylo.github.io/plotor/reference/check_or.md)
runs a compact, reproducible set of diagnostic checks for binomial
logistic regression models fitted with
`stats::glm(family = "binomial")`. It prints concise pass / fail
messages and optional explanatory details to help you decide whether to
investigate further.

## When to use this function

Use
[`check_or()`](https://craig-parylo.github.io/plotor/reference/check_or.md)
when you need to:

- **Validate model assumptions** before reporting results

- **Identify common logistic regression pitfalls** (separation,
  multicollinearity, insufficient sample size)

- **Quickly diagnose problems** without writing custom diagnostic code

- **Document model checking** in a reproducible way

**Note**:
[`check_or()`](https://craig-parylo.github.io/plotor/reference/check_or.md)
works only with `glm(family = "binomial")`. It does not support logistic
regression via other packages (e.g., `glm2`, `logistf`) or ordinal /
multinomial logistic regression.

## Inputs

- `glm_model_results`: a fitted stats::glm object with family =
  “binomial”

- `confint_fast_estimate`: logical (default = FALSE). If TRUE, uses
  faster approximate confidence interval checks

- `details`: logical (default TRUE). If TRUE, prints brief explanations
  for each check.

## Minimal example

``` r
# create a small example dataset
rows <- 400
df <- data.frame(
  outcome = rbinom(n = rows, size = 1, prob = 0.25) |> 
    factor(labels = c("Healthy", "Disease")),
  age = rnorm(n = rows, mean = 50, sd = 12),
  sex = sample(x = 0:1, size = rows, replace = TRUE) |> 
    factor(labels = c("Female", "Male")),
  smoke = sample(x = 0:2, size = rows, replace = TRUE) |> 
    factor(labels = c("Never", "Former", "Current"))
)

# fit a logistic regression model
m <- glm(
  formula = outcome ~ age + sex + smoke,
  family = "binomial",
  data = df
)

# prints messages to console
check_or(m)
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

## Diagnostic checks in detail

[`check_or()`](https://craig-parylo.github.io/plotor/reference/check_or.md)
performs six key checks. Each check prints a short status message to the
console with optional explanatory text.

### Binary outcome

**What it checks**: confirms the response variable has exactly two
distinct levels.

**Why it matters**: logistic regression requires a binary outcome. If
your outcome has more than two levels, you need multinomial or ordinal
logistic regression instead.

**What to watch for**: if this check fails, stop and reconsider your
model specification.

### Multicollinearity

**What it checks**: computer variance inflation factors (VIF) or
generalised VIF (GVIF) depending on predictor types.

**Why it matters**: high multicollinearity destabilises coefficient
estimates and inflates standard errors, making results unreliable.

**Thresholds**:

- **VIF ≥ 5** (numeric predictors): indicates problematic collinearity

- **GVIF ≥ 2** (factor predictors): indicates problematic collinearity

**What to watch for**: if flagged, inspect correlations between
predictors. Consider removing redundant variables, combining categories
or using regularisation.

### Separation detection

**What it checks**: tests for complete or quasi-complete separation in
the data.

**Why it matters**: separation occurs when a predictor perfectly (or
nearly perfectly) predicts the outcome. This causes infinite or unstable
coefficient estimates and unreliable inference.

**Two modes**:

- **`confint_fast_estimate = FALSE`** (default): uses
  [`detectseparation::detectseparation()`](https://rdrr.io/pkg/detectseparation/man/detectseparation.html)
  for robust, thorough detection. Slower but catches subtle separation
  patterns.

- **`confint_fast_estimate = TRUE`**: performs quicker checks
  (non-overlapping numeric ranges, zero-outcome levels for factors).
  Faster but may miss subtle separation.

**What to watch for**: if flagged, consider:

- removing the separating factor

- combining rare categories

- using Firth’s penalised likelihood (via `logistf` package)

### Sample size sufficiency

**What it checks**: applies the rule of thumb: **at least 10 outcome
events per estimated parameter** (including dummy variables for factor
levels).

**Why it matters**: insufficient events per parameter leads to unstable
estimates and overconfidence in results.

**Calculation**:

- Count the number of outcome events (n)

- Count the number of parameters estimated (including intercept and all
  dummy variables for factors)

- Flag if n / parameters \< 10

**What to watch for**: if flagged, consider:

- Simplifying your model (remove non-significant predictors)

- Combining factor categories

- Collecting more data

### Linearity in the logit

**What it checks**: tests whether continuous predictors have a **linear
relationship with the log-odds** of the outcome using a likelihood-ratio
test.

**Why it matters**: logistic regression assumes a linear relationship on
the log-odds scale. Non-linear relationships may require splines,
polynomials or transformation.

**Technical note**: this check uses Box-Tidwell style tests, which
transform continuous predictors to test for non-linearity. Factor
predictors are not tested because they are already categorical.

**What to watch for**: if flagged, consider:

- Adding polynomial terms (e.g., `age + I(age^2)`)

- Using restricted cubic splines (e.g., `rms::rcs(age, 3)`)

- Log-transforming the predictor if appropriate

### Influential observations

**What it checks**: identifies observations with high influence using
three criteria:

- **Cook’s distance** (overall influence on model fit)

- **Leverage** (unusual predictor values)

- **standardised residuals** (unusual outcome given predictors)

An observation is flagged if it meets **at least two of these three
criteria**.

**Why it matters**: a small number of influential observations can
disproportionately affect coefficient estimates and conclusions.

**What to watch for**: if flagged:

- Inspect the raw data for data entry errors

- Verify that observations are legitmate (not measurement errors)

- Consider robust fitting methods or justifying exclusion if appropriate

## Interpreting results

### Pass (✔)

**Meaning**: no immediate concern for that assumption

**Action**: you may proceed with confidence to report and interpret
results. See
[`vignette("table_or")`](https://craig-parylo.github.io/plotor/articles/table_or.md)
for formatting results and
[`vignette("plot_or")`](https://craig-parylo.github.io/plotor/articles/plot_or.md)
for visualisation.

### Issue (✖)

**Meaning**: a potential problem detected. Further investigation
recommended.

**Action**: use the guidance below and cross-reference the check
descriptions above.

### Recommended actions by issue

| Issue                    | Recommended actions                                                                                                                |
|--------------------------|------------------------------------------------------------------------------------------------------------------------------------|
| Binary outcome fails     | Stop. Reconsider your outcome variable. Use multinomial or ordinal logistic regression if needed.                                  |
| Multicollinearity        | Inspect predictor correlations. Remove redundant variables, combine categories or use regularisation.                              |
| Separaation detected     | Remove the separating predictor, combine rare categories or use Firth’s penalised likelihood (`logistf` package).                  |
| Low events-per-variable  | Simplify your model by removing non-significant predictors. Combine rare factor categories. Collect more data if possible.         |
| Non-linear logit         | Add polynomial terms (e.g., `age + I(age^2)`) or use splines (e.g., `rms::rcs(age, 3)`). Plot the relationship to guide decisions. |
| Influential observations | Inspect raw data for errors. Verify legitimacy of observations. Consider robust fitting or document justification for exclusion.   |

Recommended actions by issue

### Example: model with separation

To illustrate what a failed check looks like, here’s a model that
exhibits **separation**.

``` r
# create data with separation
rows <- 100
df_sep <- data.frame(
  outcome = c(rep(0, 50), rep(1, 50)) |> factor(labels = c("No", "Yes")),
  predictor1 = c(rep(0, 50), rep(1, 50)), # perfect separator
  predictor2 = rpois(n = rows, lambda = 5)
)

# fit model
m_sep <- glm(
  formula = outcome ~ predictor1 + predictor2,
  family = "binomial",
  data = df_sep
)

# run diagnostics
check_or(m_sep)
#> ── Assumption checks ───────────────────────────────────────────────────────────
#> 
#> ── Summary ──
#> 
#> ✔ The outcome variable is binary
#> ✔ Predictor variables are not highly correlated with each other
#> ✖ The outcome is separated by at least one predictor
#> ✔ The sample size is large enough
#> ✔ Continuous variables either have a linear relationship with the log-odds of
#> the outcome or are absent
#> ✖ Some observations significantly distort model estimates
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
#> ! These tests indicate there are issues (reported above) that you may wish to
#> explore further before reporting your findings.
#> 
```

In this case, the separation check fails because `predictor1` perfectly
predicts `outcome`. The output flags this issue so you can take remedial
action.

## Performance considerations

### Speed vs. Robustness

The `confint_fast_estimate` parameter controls the trade-off:

- **`confint_fast_estimate = FALSE`** (default): thorough separation
  detection using
  [`detectseparation::detectseparation()`](https://rdrr.io/pkg/detectseparation/man/detectseparation.html).
  Recommended for most users. May take longer with larger models.

- **`confint_fast_estimate = TRUE`**: faster checks using range overlap
  and zero-cell detection. Use only if model is large and speed is
  critical, accepting that subtle separation may be missed.

## Notes and limitations

- **Diagnostic aid, not definitive test**:
  [`check_or()`](https://craig-parylo.github.io/plotor/reference/check_or.md)
  is a screening tool. Use its output together with domain knowledge,
  data visualisation and thorough model checking (plots, alternative
  estimators).

- **Rule-of-thumb thresholds**: the checks use conservative,
  widely-accepted thresholds (e.g., 10 events per paramter, VIF ≥ 5).
  Your specific context may warrant different thresholds; adjust your
  interpretation accordingly.

- **Complementary tools**:
  [`check_or()`](https://craig-parylo.github.io/plotor/reference/check_or.md)
  works best alongside visual diagnostics.

- **Model-specific**: assumes a standard binomial logistic regression.
  Does not apply to conditional logistic regression, survey-weighted
  logistic regression or mixed-effects logistic regression.

## Conclusion

[`check_or()`](https://craig-parylo.github.io/plotor/reference/check_or.md)
offers a systematic, reproducible approach to validating logistic
regression model assumptions before reporting results. By catching
common pitfalls early - separation, multicollinearity, insufficient
sample size and non-linearity - it promotes more reliable and robust
statistical inference.

## See also

- [`vignette("plot_or")`](https://craig-parylo.github.io/plotor/articles/plot_or.md) -
  detailed plotting options and themes

- [`vignette("table_or")`](https://craig-parylo.github.io/plotor/articles/table_or.md) -
  formatting, gt integration and export

- [`detectseparation::detect_separation()`](https://rdrr.io/pkg/detectseparation/man/detect_separation.html) -
  separation diagnostics

- [`car::vif()`](https://rdrr.io/pkg/car/man/vif.html) -
  multicollinearity diagnostics

- `rms::rcs()` - restricted cubic splines for non-linear relationships
