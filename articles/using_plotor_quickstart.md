# plotor - Quick start

``` r
library(plotor)
set.seed(123) # reproducibility
```

Short summary: minimal reproducible workflow showing model checks, a
publication-ready odds ratio table and a forest plot.

## What this vignette covers

- Required input: a fitted glm object for binary outcome (family =
  binomial)  
- Functions shown:
  [`check_or()`](https://craig-parylo.github.io/plotor/reference/check_or.md),
  [`table_or()`](https://craig-parylo.github.io/plotor/reference/table_or.md)
  and
  [`plot_or()`](https://craig-parylo.github.io/plotor/reference/plot_or.md)  
- Output types demonstrated: tibble, gt table, ggplot2 plot.

## Minimal reproducible example

Create a small example dataset with clear factor levels and a binary
outcome:

``` r
rows <- 400
df <- data.frame(
  # the first factor level is the reference, 
  # results in odds of 'Disease' vs 'Healthy'
  outcome = rbinom(n = rows, size = 1, prob = 0.25) |> 
    factor(labels = c("Healthy", "Disease")),
  age = rnorm(n = rows, mean = 50, sd = 12),
  sex = sample(x = 0:1, size = rows, replace = TRUE) |> 
    factor(labels = c("Female", "Male")),
  smoke = sample(x = 0:2, size = rows, replace = TRUE) |> 
    factor(labels = c("Never", "Former", "Current"))
)
```

Fit a logistic regression model

``` r
m <- glm(
  formula = outcome ~ age + sex + smoke,
  data = df,
  family = "binomial"
)
```

## Run diagnostics

Using
[`check_or()`](https://craig-parylo.github.io/plotor/reference/check_or.md)

``` r
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

- Runs quick checks (binary outcome, multicollinearity, separation,
  small-sample / rare-event, linearity, influential points)

- Behaviour: prints concise human-readable messages to the console

- Issues raised here may indicate you should undertake further work to
  validate your model and understand the causes of these alerts before
  relying on any results from your model.

## Create a publication-ready table

Using
[`table_or()`](https://craig-parylo.github.io/plotor/reference/table_or.md)

``` r
# two output formats shown: gt (rendered) and tibble (for programmatic use)
table_or(m, output = "gt") # formatted HTML table
```

[TABLE]

``` r
table_or(m, output = "tibble") # programmatic output
#> # A tibble: 6 × 14
#>   label level    rows outcome outcome_rate class   estimate std.error statistic
#>   <fct> <fct>   <int>   <int>        <dbl> <chr>      <dbl>     <dbl>     <dbl>
#> 1 age   age       400      97        0.242 numeric    0.990   0.00996   -1.04  
#> 2 sex   Female    206      53        0.257 factor    NA      NA         NA     
#> 3 sex   Male      194      44        0.227 factor     0.856   0.235     -0.663 
#> 4 smoke Never     137      34        0.248 factor    NA      NA         NA     
#> 5 smoke Former    135      34        0.252 factor     1.03    0.281      0.0997
#> 6 smoke Current   128      29        0.227 factor     0.902   0.290     -0.356 
#> # ℹ 5 more variables: p.value <dbl>, conf.low <dbl>, conf.high <dbl>,
#> #   significance <chr>, comparator <dbl>
```

#### Notes:

- table_or() exponentiates coefficients to present odds ratios

- Use the tibble output when you need to further transform or combine
  results programmatically

## Forest plot of odds ratios

Display the relationships in a forest plot using
[`plot_or()`](https://craig-parylo.github.io/plotor/reference/plot_or.md)

``` r
plot_or(m)
```

![Forest plot of adjusted odds ratios with horizontal error bars showing
95% confidence intervals for predictors (age, sex and smoke). A vertical
reference line at OR = 1 marks no association; points to the
right.](using_plotor_quickstart_files/figure-html/plot-1.png)

Forest plot shows point estimates and 95% confidence intervals; the
vertical reference line at OR = 1 indicates no association

## Quick interpretation

- [`check_or()`](https://craig-parylo.github.io/plotor/reference/check_or.md)
  flags potential model issues. Concerns raised here should be a prompt
  for further investigative work to understand the implications for your
  model’s validity.

- [`table_or()`](https://craig-parylo.github.io/plotor/reference/table_or.md)
  provides odds ratios, 95% confidence intervals and p-values in a
  publication-friendly layout. Prefer reporting confidence intervals
  alongside point estimates rather than p-values alone.

- [`plot_or()`](https://craig-parylo.github.io/plotor/reference/plot_or.md)
  visualises effect sizes and uncertainty. Interpret odds ratios \> 1 as
  increased odds; odds ratios \< 1 as decreased odds. If the confidence
  interval crosses 1, there is no clear evidence of an association at
  the chosen confidence interval level (default 95%).

## See also

- [`vignette("plot_or")`](https://craig-parylo.github.io/plotor/articles/plot_or.md) -
  detailed plotting options and themes

- [`vignette("table_or")`](https://craig-parylo.github.io/plotor/articles/table_or.md) -
  formatting, gt integration and export

- [`vignette("check_or")`](https://craig-parylo.github.io/plotor/articles/check_or.md) -
  diagnostics and case studies
