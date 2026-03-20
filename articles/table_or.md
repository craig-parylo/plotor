# table_or - Publication-ready odds ratio tables

``` r
library(plotor)
set.seed(123) # reproducibility
```

## Overview

[`table_or()`](https://craig-parylo.github.io/plotor/reference/table_or.md)
automates the creation of publication-ready odds ratio tables from
logistic regression models. This vignette shows how to:

- Generate **programmatic tibble output** for downstream analysis and
  reporting

- Create **formatted HTML tables** with {gt} for manuscripts and reports

- **Customise** confidence levels, anonymisation and styling

- **Export** tables for publication

## When to use this function

Use
[`table_or()`](https://craig-parylo.github.io/plotor/reference/table_or.md)
when you need to:

- Present logistic regression results in a standardised,
  publication-ready format

- Create tables that comply with reporting guidelines, (e.g., STROBE
  guidelines for observational studies)

- Combine or compare results across multiple models

- Ensure consistent interpretation guidance for readers (via footnotes)

- Balance transparency with data privacy (via anonymisation options)

## Quick example - minimal workflow

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
```

## Programmatic output (tibble)

Use the tibble output for downstream manipulation, reporting or
combining results across models.

``` r
table_or(m, output = "tibble")
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

Key columns returned:

- **label**: Variable name or predictor group (e.g., “age”, “sex”,
  “smoke”)

- **level**: Specific level or term shown for the row (numeric variable
  name or factor level label)

- **rows**: Number of observations used to estimate the row (rows in the
  model / data subset for that term)

- **outcome**: Count of outcome events (cases) observed for that row /
  level

- **outcome_rate**: Proportion of observations with the outcome (outcome
  / rows)

- **class**: Data type of the predictor shown in this row (e.g.,
  “numeric”, “factor”)

- **estimate**: Model coefficient on the link (log-odds) scale; NA for
  comparator / reference rows

- **std.error**: Standard error of the coefficient estimate

- **statistic**: Test statistic for the coefficient (e.g., z or t value
  from the model)

- **p.value**: Two-sided p-value for the coefficient test

- **conf.low**: Lower bound of the confidence interval for the odds
  ratio (exponentiated coefficient)

- **conf.high**: Upper bound of the confidence interval for the odds
  ratio (exponentiated coefficient)

- **signfiicance**: Human-readable summary of statistical evidence
  (e.g., “Not significant”, “Significant” or “Comaparator”)

- **comparator**: Indicates reference / comparator rows (e.g., “1” for
  the first level of factors)

Estimates are shown as coefficients on the log-odds scale (estimate /
std.error / statistic / p.value) with confidence intervals presented for
exponentiated results (conf.low / conf.high).

### Interpreting the results

- **OR \> 1**: increased odds of the outcome

- **OR \< 1**: decreased odds of the outcome

- **OR = 1**: No association

- **Confidence interval crosses 1.0**: not statistically significant

## Formatted HTML table (gt)

Create a publication-ready table using gt formatting.

``` r
table_or(m, output = "gt")
```

[TABLE]

### What the gt output includes

The HTML table includes:

- **Formatted odds ratios** with confidence intervals

- **Mini forest plot** visualising effect sizes

- **Comprehensive footnotes** that help readers interpret results

### Understanding the footnotes

The footnotes clarify:

- **Interpretation of OR values**: explaining that values below 1
  indicate inverse relationships and above 1 indicate positive
  relationships

- **Variable type handling**: showing how categorical variables
  (reference vs comparison levels) and numeric variables (per-unit
  change) are presented differently

- **Significance rule**: making clear that confidence intervals crossing
  1.0 are presented differently

- **Precision**: specifying that values are shown to 4 significant
  figures

- **Metric definitions**: ensureing readers understand OR, SE, p-value
  and CI without external references

## Common customisations

### Change confidence level

Report 90% confidence intervals for sensitivity analysis or different
reporting standards:

``` r
table_or(m, output = "tibble", conf_level = 0.90)
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

### Anonymise counts for sensitive data

Improve data privacy by rounding counts to the nearest five and
suppressing counts below ten. This is useful when working with sensitive
dataset under strict information governance requirements:

``` r
table_or(m, output = "tibble", anonymise_counts = TRUE)
#> # A tibble: 6 × 14
#>   label level   rows  outcome outcome_rate class   estimate std.error statistic
#>   <fct> <fct>   <chr> <chr>          <dbl> <chr>      <dbl>     <dbl>     <dbl>
#> 1 age   age     400   95             0.242 numeric    0.990   0.00996   -1.04  
#> 2 sex   Female  205   55             0.257 factor    NA      NA         NA     
#> 3 sex   Male    195   45             0.227 factor     0.856   0.235     -0.663 
#> 4 smoke Never   135   35             0.248 factor    NA      NA         NA     
#> 5 smoke Former  135   35             0.252 factor     1.03    0.281      0.0997
#> 6 smoke Current 130   30             0.227 factor     0.902   0.290     -0.356 
#> # ℹ 5 more variables: p.value <dbl>, conf.low <dbl>, conf.high <dbl>,
#> #   significance <chr>, comparator <dbl>
```

### Combine results from multiple models

Compare models by binding tibble outputs:

``` r
# fit a second model
m2 <- glm(
  formula = outcome ~ age + sex,
  family = "binomial",
  data = df
)

# combine results
combined_results <-
  dplyr::bind_rows(
    dplyr::bind_cols(model = "Model 1", table_or(m)),
    dplyr::bind_cols(model = "Model 2", table_or(m2))
  )

head(combined_results)
#> # A tibble: 6 × 15
#>   model   label level    rows outcome outcome_rate class   estimate std.error
#>   <chr>   <fct> <fct>   <int>   <int>        <dbl> <chr>      <dbl>     <dbl>
#> 1 Model 1 age   age       400      97        0.242 numeric    0.990   0.00996
#> 2 Model 1 sex   Female    206      53        0.257 factor    NA      NA      
#> 3 Model 1 sex   Male      194      44        0.227 factor     0.856   0.235  
#> 4 Model 1 smoke Never     137      34        0.248 factor    NA      NA      
#> 5 Model 1 smoke Former    135      34        0.252 factor     1.03    0.281  
#> 6 Model 1 smoke Current   128      29        0.227 factor     0.902   0.290  
#> # ℹ 6 more variables: statistic <dbl>, p.value <dbl>, conf.low <dbl>,
#> #   conf.high <dbl>, significance <chr>, comparator <dbl>
```

## Exporting tables

### Export to HTML file

``` r
gt_table <- table_or(m, output = "gt")
gt::gtsave(data = gt_table, filename = "odds_ratios.html")
```

### Export to Word document

``` r
gt_table <- table_or(m, output = "gt")
gt::gtsave(data = gt_table, filename = "odds_ratios.docx")
```

### Export to CSV (tibble)

``` r
readr::write_csv(x = table_or(m), file = "odds_ratios.csv")
```

### Tips for publication

- **Choose your output format wisely**: use “tibble” for flexibility and
  downstream analysis; use “gt” for direct inclusion in manuscripts or
  reports

- **Select relevant columns**: not all columns are needed for
  publication; consider your audience and journal requirements

- **Check the confidence interval rule**: always verify that your
  signifiance conslusions match the CI crossing rule (CI does not cross
  1.0 = significant)

- **Report the confidence level**: specify whether you’re using 95%
  (default), 90% or another confidence level

- **Use anonymisation for sensitive data**: when sharing tables with
  restricted data, use `anonymise_counts = TRUE`

## Conclusion

The
[`table_or()`](https://craig-parylo.github.io/plotor/reference/table_or.md)
function automates the creation of publication-ready odds ratio tables
from logistic regression models, eliminating manual calculation and
formatting while ensuring consistent presentation of results with
appropriate statistical annotations.

## See also

- [`vignette("plot_or")`](https://craig-parylo.github.io/plotor/articles/plot_or.md) -
  detailed plotting options and themes

- [`vignette("check_or")`](https://craig-parylo.github.io/plotor/articles/check_or.md) -
  diagnostics and case studies

- [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) - formatted
  table creation
