# Creating publication-ready labels for your outputs

``` r
library(plotor)
set.seed(123)
```

### Overview

By default,
[`plot_or()`](https://craig-parylo.github.io/plotor/reference/plot_or.md)
and
[`table_or()`](https://craig-parylo.github.io/plotor/reference/table_or.md)
display variable names exactly as they appear in your model. Variable
names like `age_group_35_44` or `smoking_status_yes` are **technical and
not suitable for publication-ready outputs**.

The solution is to **attach human-readable labels to variables**. When
you do, `plotor` automatically uses these labels in plots and tables
instead of the raw variable names. This approach is **much cleaner than
manually editing outputs** after generation.

## Example 1: Oesophageal Cancer Study

### The dataset

This [case-control
study](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/esoph.html)
examined oesophageal cancer in Ile-et-Vilaine, France. It contains:

| Variable | Description                             |
|----------|-----------------------------------------|
| `Group`  | Case (cancer) or Control (disease-free) |
| `agegp`  | Age group of participant                |
| `alcgp`  | Alcohol consumption (grams per day)     |
| `tobgp`  | Tobacco consumption (grams per day)     |

### Preparing the data

``` r
# prepare the dataset for modelling
df <- 
  datasets::esoph |> 
  # convert aggregated data to tidy observational data
  tidyr::pivot_longer(
    cols = c(ncases, ncontrols),
    names_to = 'Group',
    values_to = 'people'
  ) |> 
  tidyr::uncount(weights = people) |> 
  # prepare the variables
  dplyr::mutate(
    # convert the intervention group to a factor
    Group = Group |> 
      dplyr::recode_values(
        "ncases" ~ "Case",
        "ncontrols" ~ "Control"
      ) |> 
        factor(levels = c("Control", "Case")),
    # remove ordering from these predictors
    agegp = agegp |> factor(ordered = FALSE),
    alcgp = alcgp |> factor(ordered = FALSE),
    tobgp = tobgp |> factor(ordered = FALSE)
  )

# preview the data
df |> dplyr::glimpse()
#> Rows: 975
#> Columns: 4
#> $ agegp <fct> 25-34, 25-34, 25-34, 25-34, 25-34, 25-34, 25-34, 25-34, 25-34, 2…
#> $ alcgp <fct> 0-39g/day, 0-39g/day, 0-39g/day, 0-39g/day, 0-39g/day, 0-39g/day…
#> $ tobgp <fct> 0-9g/day, 0-9g/day, 0-9g/day, 0-9g/day, 0-9g/day, 0-9g/day, 0-9g…
#> $ Group <fct> Control, Control, Control, Control, Control, Control, Control, C…
```

### Without labels

``` r
m <- glm(
  data = df,
  family = "binomial",
  formula = Group ~ agegp + alcgp + tobgp
)

# plot the odds ratio with a customised title
plot_or(m)
```

![](improving_readability_files/figure-html/ex1%20-%20model%20without%20labels-1.png)

Notice how the plot uses technical variable names like `alcgp` and
`tobgp` which are not immediately clear to readers.

### Adding labels with `{labelled}`

To make your outputs more readable, attach descriptive labels to your
variables before modelling.

First, ensure the package is installed.

``` r
install.packages("labelled")
library(labelled)
```

``` r
# create a list that matches variables with user-friendly labels
var_labels <- list(
  agegp = "Age group",
  alcgp = "Alcohol consumption",
  tobgp = "Tobacco consumption",
  Group = "Likelihood of developing oesophageal cancer"
)

# apply these variables to our data
labelled::var_label(df) <- var_labels

# preview the data with labels applied
labelled::look_for(df)
#>  pos variable label                                    col_type missing
#>  1   agegp    Age group                                fct      0      
#>                                                                        
#>                                                                        
#>                                                                        
#>                                                                        
#>                                                                        
#>  2   alcgp    Alcohol consumption                      fct      0      
#>                                                                        
#>                                                                        
#>                                                                        
#>  3   tobgp    Tobacco consumption                      fct      0      
#>                                                                        
#>                                                                        
#>                                                                        
#>  4   Group    Likelihood of developing oesophageal ca~ fct      0      
#>                                                                        
#>  values   
#>  25-34    
#>  35-44    
#>  45-54    
#>  55-64    
#>  65-74    
#>  75+      
#>  0-39g/day
#>  40-79    
#>  80-119   
#>  120+     
#>  0-9g/day 
#>  10-19    
#>  20-29    
#>  30+      
#>  Control  
#>  Case
```

#### Forest plots with labels

Now fit the model using the labelled data:

``` r
m <- glm(
  data = df,
  family = "binomial",
  formula = Group ~ agegp + alcgp + tobgp
)

# plot the odds ratio with a customised title
plot_or(m)
```

![](improving_readability_files/figure-html/ex1%20-%20model%20with%20labels-1.png)

The plot is now much more **reader-friendly**:

- The outcome label (“Likelihood of developing oesophageal cancer”)
  appears in the title

- Predictor labels (“Age group”, “Alcohol consumption”, “Tobacco
  consumption”) replace technical variable names

#### Data tables with labels

[`table_or()`](https://craig-parylo.github.io/plotor/reference/table_or.md)
also respects variable labels:

``` r
table_or(m, output = "gt", assumption_checks = FALSE)
```

[TABLE]

## Example 2: Post-endoscopic pancreatitis study

### The dataset

The `indo_rct` dataset contains details from **602 patients** in a
randomised controlled study examining indomethacin vs placebo for
preventing post-endoscopic pancreatitis.

``` r
# prepare the dataset for modelling
df <- medicaldata::indo_rct |> 
  tibble::as_tibble() |> 
  # clean up factor levels
  dplyr::mutate(
    rx = forcats::fct_recode(
      .f = rx, 
      Placebo = "0_placebo", Indomethacin = "1_indomethacin"),
    pep = forcats::fct_recode(
      .f = pep,
      No = "0_no", Yes = "1_yes"
    ),
    amp = forcats::fct_recode(
      .f = amp,
      No = "0_no", Yes = "1_yes"
    )
  )

# fit the model without labels
m <- stats::glm(
  formula = outcome ~  rx + pep + amp,
  family = "binomial",
  data = df
)

plot_or(m, assumption_checks = FALSE)
```

![](improving_readability_files/figure-html/ex2%20-%20dataset%20wrangling-1.png)

### Using `{Hmisc}` for labels

An alternative to {labelled} is the
[{Hmisc}](https://hbiostat.org/r/hmisc/) package, which is particularly
useful if you’re already using other {Hmisc} functions like
[`describe()`](https://rdrr.io/pkg/Hmisc/man/describe.html) for summary
statistics.

Install it with:

``` r
install.packages("Hmisc")
library(Hmisc)
```

Attach labels using the
[`label()`](https://rdrr.io/pkg/Hmisc/man/label.html) function:

``` r
# label the variables
Hmisc::label(df$outcome) <- "Likelihood of post-ERCP pancreatitis"
Hmisc::label(df$rx) <- "Treatment arm"
Hmisc::label(df$pep) <- "Previous post-ERCP pancreatitis (PEP)"
Hmisc::label(df$amp) <- "Ampullectomy performed"

# fit the model with labelled data
m <- stats::glm(
  formula = outcome ~  rx + pep + amp,
  family = "binomial",
  data = df
)

# plot
plot_or(m, assumption_checks = FALSE)
```

![](improving_readability_files/figure-html/ex2%20-%20labelling%20with%20Hmisc-1.png)

This plot now clearly shows that **treatment with Indomethacin** has a
protective effect against pancreatitis, whereas a **history of
pancreatitis** and **ampullectomy** are both associated with increased
risk.

The same labels also appear in covariate tables:

``` r
table_or(m, output = "gt", assumption_checks = FALSE)
```

[TABLE]

## Best practices for labelling

### Be descriptive but concise

Keep labels clear and specific, but not verbose:

| **✓ Good**                       | **✗ Avoid**                           |
|----------------------------------|---------------------------------------|
| “Age Group (years)”              | “The age of the participant in years” |
| “Systolic Blood Pressure (mmHg)” | “BP_sys”                              |
| “Smoking Status”                 | “smoking_status_yes”                  |

### Include units where relevant

Always specify units in parentheses:

``` r
var_labels <- list(
  wt = "Weight (kg)",
  ht = "Height (cm)",
  bp_sys = "Systolic Blood Pressure (mmHg)"
)
```

### Use consistent formatting

Apply consistent capitalisation and punctuation across all labels:

``` r
# consistent approach to capitalisation
var_labels <- list(
  ag_gp = "Age Group",
  sm_status = "Smoking Status",
  ed_level = "Education Level"
)
```

#### Label factor levels clearly

Make factor level labels explicit and unambiguous:

``` r
df <-
  data.frame(
    education = sample(
      x = 1:3,
      size = 10,
      replace = TRUE
    ) |>
      factor(
        labels = c(
          "Primary school",
          "Secondary school",
          "University degree"
        )
      )
  )
```

### Preserving labels in your workflow

#### R-native formats preserve labels

Labels are preserved when saving and reloading with `.Rds` or `.RData`:

``` r
# labels are preserved
saveRDS(df, "my_data.Rds")
df <- readRDS("my_data.Rds")
```

#### CSV files lose labels

**Labels are lost** when reading from CSV or other text formats. To
preserve labels, use one of these approaches:

1.  **Use R-native formats** like `.Rds` or `.RData`

2.  **Re-apply labels** after reading CSV files

3.  **Store labels separately** in a data dictionary and apply them in
    your analysis script

### See also

- [`vignette("table_or")`](https://craig-parylo.github.io/plotor/articles/table_or.md) -
  formatting results tables and exporting with {gt}

- [`vignette("check_or")`](https://craig-parylo.github.io/plotor/articles/check_or.md) -
  diagnostics and model validation

- [labelled](https://larmarange.github.io/labelled/) - comprehensive
  labelling package

- [haven](https://haven.tidyverse.org) - import labelled data from SPSS,
  Stata or SAS

- [Hmisc](https://hbiostat.org/r/hmisc/) - labelling and statistical
  functions commonly used in epidemiology and biostatistics
