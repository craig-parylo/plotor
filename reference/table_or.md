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

Odds Ratio summary table with 95% Confidence Interval

Characteristic¹

Odds Ratio (OR)²

95% Confidence Interval (CI)³

OR Plot

Level

N

n

Rate

Class

OR

SE

p

Lower

Upper

Significance

Class

1st

325

203

62.46%

factor

—

—

—

—

—

Comparator

 

2nd

285

118

41.4%

factor

0.3613

0.1960

2.05 × 10⁻⁷

0.2453

0.5292

Significant

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB3aWR0aD0iMTI3LjU2cHQiIGhlaWdodD0iMTQuMTdwdCIgdmlld2JveD0iMCAwIDEyNy41NiAxNC4xNyI+PGcgY2xhc3M9InN2Z2xpdGUiPjxkZWZzPjwvZGVmcz48cmVjdCB3aWR0aD0iMTAwJSIgaGVpZ2h0PSIxMDAlIiBzdHlsZT0ic3Ryb2tlLWxpbmVjYXA6IHJvdW5kOyBzdHJva2UtbGluZWpvaW46IHJvdW5kOyBzdHJva2UtbWl0ZXJsaW1pdDogMTAuMDA7IHN0cm9rZTogbm9uZTsgZmlsbDogbm9uZTsiIC8+PGRlZnM+PGNsaXBwYXRoIGlkPSJjcE1DNHdNSHd4TWpjdU5UWjhNQzR3TUh3eE5DNHhOdz09Ij48cmVjdCB4PSIwLjAwIiB5PSIwLjAwIiB3aWR0aD0iMTI3LjU2IiBoZWlnaHQ9IjE0LjE3IiBzdHlsZT0iZmlsbDogbm9uZTsgc3Ryb2tlOiAjMDAwMDAwOyBzdHJva2UtbGluZWNhcDogcm91bmQ7IHN0cm9rZS1saW5lam9pbjogcm91bmQ7IHN0cm9rZS1taXRlcmxpbWl0OiAxMC4wMDsiIC8+PC9jbGlwcGF0aD48L2RlZnM+PGcgY2xpcC1wYXRoPSJ1cmwoI2NwTUM0d01Id3hNamN1TlRaOE1DNHdNSHd4TkM0eE53PT0pIj48dGV4dCB4PSI3Ni45NyIgeT0iMTEuMjYiIHN0eWxlPSJ3aGl0ZS1zcGFjZTogcHJlOyBmb250LXNpemU6IDAuMDBweDsgZm9udC13ZWlnaHQ6IGJvbGQ7IGZvbnQtZmFtaWx5OiAmIzM5O05pbWJ1cyBNb25vIFBTJiMzOTs7IiB0ZXh0bGVuZ3RoPSIwLjU5cHgiIGxlbmd0aGFkanVzdD0ic3BhY2luZ0FuZEdseXBocyI+MDwvdGV4dD48bGluZSB4MT0iNzYuOTciIHkxPSIxNC4xNyIgeDI9Ijc2Ljk3IiB5Mj0iMC4wMDAwMDAwMDAwMDAwMDE4IiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvbGluZT48bGluZSB4MT0iNDQuOTUiIHkxPSI4LjY2IiB4Mj0iNjIuNDciIHkyPSI4LjY2IiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAyLjEzOyBzdHJva2U6ICNCRUJFQkU7IHN0cm9rZS1vcGFjaXR5OiAwLjc1OyI+PC9saW5lPjxjaXJjbGUgY3g9IjUzLjc3IiBjeT0iOC42NiIgcj0iMi42NyIgc3R5bGU9InN0cm9rZS1saW5lY2FwOiByb3VuZDsgc3Ryb2tlLWxpbmVqb2luOiByb3VuZDsgc3Ryb2tlLW1pdGVybGltaXQ6IDEwLjAwOyBzdHJva2Utd2lkdGg6IDEuMDY7IHN0cm9rZTogI0ZGRkZGRjsgZmlsbDogIzAwMDAwMDsiPjwvY2lyY2xlPjwvZz48L2c+PC9zdmc+)

3rd

706

178

25.21%

factor

0.1690

0.1716

3.69 × 10⁻²⁵

0.1203

0.2358

Significant

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB3aWR0aD0iMTI3LjU2cHQiIGhlaWdodD0iMTQuMTdwdCIgdmlld2JveD0iMCAwIDEyNy41NiAxNC4xNyI+PGcgY2xhc3M9InN2Z2xpdGUiPjxkZWZzPjwvZGVmcz48cmVjdCB3aWR0aD0iMTAwJSIgaGVpZ2h0PSIxMDAlIiBzdHlsZT0ic3Ryb2tlLWxpbmVjYXA6IHJvdW5kOyBzdHJva2UtbGluZWpvaW46IHJvdW5kOyBzdHJva2UtbWl0ZXJsaW1pdDogMTAuMDA7IHN0cm9rZTogbm9uZTsgZmlsbDogbm9uZTsiIC8+PGRlZnM+PGNsaXBwYXRoIGlkPSJjcE1DNHdNSHd4TWpjdU5UWjhNQzR3TUh3eE5DNHhOdz09Ij48cmVjdCB4PSIwLjAwIiB5PSIwLjAwIiB3aWR0aD0iMTI3LjU2IiBoZWlnaHQ9IjE0LjE3IiBzdHlsZT0iZmlsbDogbm9uZTsgc3Ryb2tlOiAjMDAwMDAwOyBzdHJva2UtbGluZWNhcDogcm91bmQ7IHN0cm9rZS1saW5lam9pbjogcm91bmQ7IHN0cm9rZS1taXRlcmxpbWl0OiAxMC4wMDsiIC8+PC9jbGlwcGF0aD48L2RlZnM+PGcgY2xpcC1wYXRoPSJ1cmwoI2NwTUM0d01Id3hNamN1TlRaOE1DNHdNSHd4TkM0eE53PT0pIj48dGV4dCB4PSI3Ni45NyIgeT0iMTEuMjYiIHN0eWxlPSJ3aGl0ZS1zcGFjZTogcHJlOyBmb250LXNpemU6IDAuMDBweDsgZm9udC13ZWlnaHQ6IGJvbGQ7IGZvbnQtZmFtaWx5OiAmIzM5O05pbWJ1cyBNb25vIFBTJiMzOTs7IiB0ZXh0bGVuZ3RoPSIwLjU5cHgiIGxlbmd0aGFkanVzdD0ic3BhY2luZ0FuZEdseXBocyI+MDwvdGV4dD48bGluZSB4MT0iNzYuOTciIHkxPSIxNC4xNyIgeDI9Ijc2Ljk3IiB5Mj0iMC4wMDAwMDAwMDAwMDAwMDE4IiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvbGluZT48bGluZSB4MT0iMjguNzIiIHkxPSI4LjY2IiB4Mj0iNDQuMDUiIHkyPSI4LjY2IiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAyLjEzOyBzdHJva2U6ICNCRUJFQkU7IHN0cm9rZS1vcGFjaXR5OiAwLjc1OyI+PC9saW5lPjxjaXJjbGUgY3g9IjM2LjQ2IiBjeT0iOC42NiIgcj0iMi42NyIgc3R5bGU9InN0cm9rZS1saW5lY2FwOiByb3VuZDsgc3Ryb2tlLWxpbmVqb2luOiByb3VuZDsgc3Ryb2tlLW1pdGVybGltaXQ6IDEwLjAwOyBzdHJva2Utd2lkdGg6IDEuMDY7IHN0cm9rZTogI0ZGRkZGRjsgZmlsbDogIzAwMDAwMDsiPjwvY2lyY2xlPjwvZz48L2c+PC9zdmc+)

Crew

885

212

23.95%

factor

0.4241

0.1573

5.00 × 10⁻⁸

0.3115

0.5775

Significant

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB3aWR0aD0iMTI3LjU2cHQiIGhlaWdodD0iMTQuMTdwdCIgdmlld2JveD0iMCAwIDEyNy41NiAxNC4xNyI+PGcgY2xhc3M9InN2Z2xpdGUiPjxkZWZzPjwvZGVmcz48cmVjdCB3aWR0aD0iMTAwJSIgaGVpZ2h0PSIxMDAlIiBzdHlsZT0ic3Ryb2tlLWxpbmVjYXA6IHJvdW5kOyBzdHJva2UtbGluZWpvaW46IHJvdW5kOyBzdHJva2UtbWl0ZXJsaW1pdDogMTAuMDA7IHN0cm9rZTogbm9uZTsgZmlsbDogbm9uZTsiIC8+PGRlZnM+PGNsaXBwYXRoIGlkPSJjcE1DNHdNSHd4TWpjdU5UWjhNQzR3TUh3eE5DNHhOdz09Ij48cmVjdCB4PSIwLjAwIiB5PSIwLjAwIiB3aWR0aD0iMTI3LjU2IiBoZWlnaHQ9IjE0LjE3IiBzdHlsZT0iZmlsbDogbm9uZTsgc3Ryb2tlOiAjMDAwMDAwOyBzdHJva2UtbGluZWNhcDogcm91bmQ7IHN0cm9rZS1saW5lam9pbjogcm91bmQ7IHN0cm9rZS1taXRlcmxpbWl0OiAxMC4wMDsiIC8+PC9jbGlwcGF0aD48L2RlZnM+PGcgY2xpcC1wYXRoPSJ1cmwoI2NwTUM0d01Id3hNamN1TlRaOE1DNHdNSHd4TkM0eE53PT0pIj48dGV4dCB4PSI3Ni45NyIgeT0iMTEuMjYiIHN0eWxlPSJ3aGl0ZS1zcGFjZTogcHJlOyBmb250LXNpemU6IDAuMDBweDsgZm9udC13ZWlnaHQ6IGJvbGQ7IGZvbnQtZmFtaWx5OiAmIzM5O05pbWJ1cyBNb25vIFBTJiMzOTs7IiB0ZXh0bGVuZ3RoPSIwLjU5cHgiIGxlbmd0aGFkanVzdD0ic3BhY2luZ0FuZEdseXBocyI+MDwvdGV4dD48bGluZSB4MT0iNzYuOTciIHkxPSIxNC4xNyIgeDI9Ijc2Ljk3IiB5Mj0iMC4wMDAwMDAwMDAwMDAwMDE4IiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvbGluZT48bGluZSB4MT0iNTAuNDAiIHkxPSI4LjY2IiB4Mj0iNjQuNDYiIHkyPSI4LjY2IiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAyLjEzOyBzdHJva2U6ICNCRUJFQkU7IHN0cm9rZS1vcGFjaXR5OiAwLjc1OyI+PC9saW5lPjxjaXJjbGUgY3g9IjU3LjQzIiBjeT0iOC42NiIgcj0iMi42NyIgc3R5bGU9InN0cm9rZS1saW5lY2FwOiByb3VuZDsgc3Ryb2tlLWxpbmVqb2luOiByb3VuZDsgc3Ryb2tlLW1pdGVybGltaXQ6IDEwLjAwOyBzdHJva2Utd2lkdGg6IDEuMDY7IHN0cm9rZTogI0ZGRkZGRjsgZmlsbDogIzAwMDAwMDsiPjwvY2lyY2xlPjwvZz48L2c+PC9zdmc+)

Sex

Female

470

344

73.19%

factor

—

—

—

—

—

Comparator

 

Male

1,731

367

21.2%

factor

0.08892

0.1404

1.43 × 10⁻⁶⁶

0.06725

0.1166

Significant

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB3aWR0aD0iMTI3LjU2cHQiIGhlaWdodD0iMTQuMTdwdCIgdmlld2JveD0iMCAwIDEyNy41NiAxNC4xNyI+PGcgY2xhc3M9InN2Z2xpdGUiPjxkZWZzPjwvZGVmcz48cmVjdCB3aWR0aD0iMTAwJSIgaGVpZ2h0PSIxMDAlIiBzdHlsZT0ic3Ryb2tlLWxpbmVjYXA6IHJvdW5kOyBzdHJva2UtbGluZWpvaW46IHJvdW5kOyBzdHJva2UtbWl0ZXJsaW1pdDogMTAuMDA7IHN0cm9rZTogbm9uZTsgZmlsbDogbm9uZTsiIC8+PGRlZnM+PGNsaXBwYXRoIGlkPSJjcE1DNHdNSHd4TWpjdU5UWjhNQzR3TUh3eE5DNHhOdz09Ij48cmVjdCB4PSIwLjAwIiB5PSIwLjAwIiB3aWR0aD0iMTI3LjU2IiBoZWlnaHQ9IjE0LjE3IiBzdHlsZT0iZmlsbDogbm9uZTsgc3Ryb2tlOiAjMDAwMDAwOyBzdHJva2UtbGluZWNhcDogcm91bmQ7IHN0cm9rZS1saW5lam9pbjogcm91bmQ7IHN0cm9rZS1taXRlcmxpbWl0OiAxMC4wMDsiIC8+PC9jbGlwcGF0aD48L2RlZnM+PGcgY2xpcC1wYXRoPSJ1cmwoI2NwTUM0d01Id3hNamN1TlRaOE1DNHdNSHd4TkM0eE53PT0pIj48dGV4dCB4PSI3Ni45NyIgeT0iMTEuMjYiIHN0eWxlPSJ3aGl0ZS1zcGFjZTogcHJlOyBmb250LXNpemU6IDAuMDBweDsgZm9udC13ZWlnaHQ6IGJvbGQ7IGZvbnQtZmFtaWx5OiAmIzM5O05pbWJ1cyBNb25vIFBTJiMzOTs7IiB0ZXh0bGVuZ3RoPSIwLjU5cHgiIGxlbmd0aGFkanVzdD0ic3BhY2luZ0FuZEdseXBocyI+MDwvdGV4dD48bGluZSB4MT0iNzYuOTciIHkxPSIxNC4xNyIgeDI9Ijc2Ljk3IiB5Mj0iMC4wMDAwMDAwMDAwMDAwMDE4IiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvbGluZT48bGluZSB4MT0iMTUuNDYiIHkxPSI4LjY2IiB4Mj0iMjguMDEiIHkyPSI4LjY2IiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAyLjEzOyBzdHJva2U6ICNCRUJFQkU7IHN0cm9rZS1vcGFjaXR5OiAwLjc1OyI+PC9saW5lPjxjaXJjbGUgY3g9IjIxLjgzIiBjeT0iOC42NiIgcj0iMi42NyIgc3R5bGU9InN0cm9rZS1saW5lY2FwOiByb3VuZDsgc3Ryb2tlLWxpbmVqb2luOiByb3VuZDsgc3Ryb2tlLW1pdGVybGltaXQ6IDEwLjAwOyBzdHJva2Utd2lkdGg6IDEuMDY7IHN0cm9rZTogI0ZGRkZGRjsgZmlsbDogIzAwMDAwMDsiPjwvY2lyY2xlPjwvZz48L2c+PC9zdmc+)

Age

Adult

2,092

654

31.26%

factor

—

—

—

—

—

Comparator

 

Child

109

57

52.29%

factor

2.891

0.2440

1.36 × 10⁻⁵

1.792

4.671

Significant

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB3aWR0aD0iMTI3LjU2cHQiIGhlaWdodD0iMTQuMTdwdCIgdmlld2JveD0iMCAwIDEyNy41NiAxNC4xNyI+PGcgY2xhc3M9InN2Z2xpdGUiPjxkZWZzPjwvZGVmcz48cmVjdCB3aWR0aD0iMTAwJSIgaGVpZ2h0PSIxMDAlIiBzdHlsZT0ic3Ryb2tlLWxpbmVjYXA6IHJvdW5kOyBzdHJva2UtbGluZWpvaW46IHJvdW5kOyBzdHJva2UtbWl0ZXJsaW1pdDogMTAuMDA7IHN0cm9rZTogbm9uZTsgZmlsbDogbm9uZTsiIC8+PGRlZnM+PGNsaXBwYXRoIGlkPSJjcE1DNHdNSHd4TWpjdU5UWjhNQzR3TUh3eE5DNHhOdz09Ij48cmVjdCB4PSIwLjAwIiB5PSIwLjAwIiB3aWR0aD0iMTI3LjU2IiBoZWlnaHQ9IjE0LjE3IiBzdHlsZT0iZmlsbDogbm9uZTsgc3Ryb2tlOiAjMDAwMDAwOyBzdHJva2UtbGluZWNhcDogcm91bmQ7IHN0cm9rZS1saW5lam9pbjogcm91bmQ7IHN0cm9rZS1taXRlcmxpbWl0OiAxMC4wMDsiIC8+PC9jbGlwcGF0aD48L2RlZnM+PGcgY2xpcC1wYXRoPSJ1cmwoI2NwTUM0d01Id3hNamN1TlRaOE1DNHdNSHd4TkM0eE53PT0pIj48dGV4dCB4PSI3Ni45NyIgeT0iMTEuMjYiIHN0eWxlPSJ3aGl0ZS1zcGFjZTogcHJlOyBmb250LXNpemU6IDAuMDBweDsgZm9udC13ZWlnaHQ6IGJvbGQ7IGZvbnQtZmFtaWx5OiAmIzM5O05pbWJ1cyBNb25vIFBTJiMzOTs7IiB0ZXh0bGVuZ3RoPSIwLjU5cHgiIGxlbmd0aGFkanVzdD0ic3BhY2luZ0FuZEdseXBocyI+MDwvdGV4dD48bGluZSB4MT0iNzYuOTciIHkxPSIxNC4xNyIgeDI9Ijc2Ljk3IiB5Mj0iMC4wMDAwMDAwMDAwMDAwMDE4IiBzdHlsZT0ic3Ryb2tlLXdpZHRoOiAxLjA3OyBzdHJva2UtbGluZWNhcDogYnV0dDsiPjwvbGluZT48bGluZSB4MT0iOTAuMjciIHkxPSI4LjY2IiB4Mj0iMTEyLjEwIiB5Mj0iOC42NiIgc3R5bGU9InN0cm9rZS13aWR0aDogMi4xMzsgc3Ryb2tlOiAjQkVCRUJFOyBzdHJva2Utb3BhY2l0eTogMC43NTsiPjwvbGluZT48Y2lyY2xlIGN4PSIxMDEuMTYiIGN5PSI4LjY2IiByPSIyLjY3IiBzdHlsZT0ic3Ryb2tlLWxpbmVjYXA6IHJvdW5kOyBzdHJva2UtbGluZWpvaW46IHJvdW5kOyBzdHJva2UtbWl0ZXJsaW1pdDogMTAuMDA7IHN0cm9rZS13aWR0aDogMS4wNjsgc3Ryb2tlOiAjRkZGRkZGOyBmaWxsOiAjMDAwMDAwOyI+PC9jaXJjbGU+PC9nPjwvZz48L3N2Zz4=)

¹ **Characteristics** are the explanatory variables in the logistic
regression analysis. For categorical variables the first characteristic
is designated as a reference against which the others are compared. For
numeric variables the results indicate a change per single unit
increase.

*Level* - the name or the description of the explanatory variable.

*N* - the number of observations examined.

*n* - the number of observations resulting in the outcome of interest.

*Rate* - the proportion of observations resulting in the outcome of
interest (n / N).

*Class* - description of the data type.

² **Odds Ratios** estimate the relative *odds* of an outcome with
reference to the *Characteristic*. For categorical data the first level
is the reference against which the odds of other levels are compared.
Numerical characteristics indicate the change in *OR* for each
additional increase of one unit in the variable.

*OR* - The Odds Ratio point estimate - values below 1 indicate an
inverse relationship whereas values above 1 indicate a positive
relationship. Values shown to 4 significant figures.

*SE* - Standard Error of the point estimate. Values shown to 4
significant figures.

*p* - The p-value estimate based on the residual Chi-squared statistic.

³ **Confidence Interval** - the range of values likely to contain the
*OR* in 95% of cases if this study were to be repeated multiple times.
If the *CI* touches or crosses the value 1 then it is unlikely the
*Characteristic* is significantly associated with the outcome.

*Lower* & *Upper* - The range of values comprising the *CI*, shown to 4
significant figures.

*Significance* - The statistical significance indicated by the *CI*,
*Significant* where the *CI* does not touch or cross the value 1.
