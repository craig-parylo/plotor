# Changelog

## plotor v0.8.0 (2025-12-04)

### Enhancements

- **Univariable Odds Ratios**  
  Added functionality to produce univariable odds ratios alongside
  multivariate model odds ratios in
  [`plot_or()`](https://craig-parylo.github.io/plotor/reference/plot_or.md)
  and
  [`table_or()`](https://craig-parylo.github.io/plotor/reference/table_or.md).
  This allows for direct comparison between predictors in isolation and
  as part of a full model.  
  ([\#59](https://github.com/craig-parylo/plotor/issues/59))

- **Assumption Checks Control**  
  Both
  [`plot_or()`](https://craig-parylo.github.io/plotor/reference/plot_or.md)
  and
  [`table_or()`](https://craig-parylo.github.io/plotor/reference/table_or.md)
  now include a parameter to enable or disable assumption checks, giving
  experienced analysts more control over feedback from these
  functions.  
  ([\#75](https://github.com/craig-parylo/plotor/issues/75))

- **Check for Linearity**  
  Continuous predictors are now checked for linearity in relation to the
  log-odds of the outcome, improving model diagnostics.  
  ([\#21](https://github.com/craig-parylo/plotor/issues/21))

- **Privacy Options**  
  A new option in
  [`table_or()`](https://craig-parylo.github.io/plotor/reference/table_or.md)
  allows suppression of low counts (below a user-defined threshold) and
  rounding of other counts, to help protect sensitive data.  
  ([\#58](https://github.com/craig-parylo/plotor/issues/58))

- **Contextual Guidance for Model Assumptions**  
  Now, when assumption checks fail in
  [`plot_or()`](https://craig-parylo.github.io/plotor/reference/plot_or.md)
  or
  [`table_or()`](https://craig-parylo.github.io/plotor/reference/table_or.md),
  users are prompted to run
  [`check_or()`](https://craig-parylo.github.io/plotor/reference/check_or.md)
  for detailed diagnostics (unless it has just been used).  
  ([\#76](https://github.com/craig-parylo/plotor/issues/76))

### Bug Fixes

- **Performance of Confidence Interval Calculation**  
  Improved processing speed of the `confint_fast_estimate` parameter for
  large datasets by optimizing the assumption check functions.  
  ([\#57](https://github.com/craig-parylo/plotor/issues/57))

- **Sample Size Check with Complete Separation**  
  Resolved errors in `assumption_sample_size()` when the model exhibits
  complete separation by better handling of `NA` values in outcome
  counts.  
  ([\#73](https://github.com/craig-parylo/plotor/issues/73))

- **Documentation Correction**  
  Documentation for
  [`anonymise_count_values()`](https://craig-parylo.github.io/plotor/reference/anonymise_count_values.md)
  has been corrected: this internal function is no longer exported in
  the help files.  
  ([\#84](https://github.com/craig-parylo/plotor/issues/84))

## plotor 0.7.0

CRAN release: 2025-07-01

#### Compatibility and testing fixes:

- **Test suite**
  ([\#68](https://github.com/craig-parylo/plotor/issues/68))

  Addressed issues where snapshot tests failed depending on the
  installed version of ggplot2. Snapshots produced by different ggplot2
  versions were causing test failures, especially with
  [`vdiffr::expect_doppelganger()`](https://vdiffr.r-lib.org/reference/expect_doppelganger.html).

  The temporary solution was to suspend these visual comparison tests to
  avoid unnecessary failures for users not on the latest ggplot2.

- **Readiness for upcoming {ggplot2} release**
  ([\#65](https://github.com/craig-parylo/plotor/issues/65))

  Investigated and resolved failures with the upcoming major release of
  ggplot2, ensuring that the package’s examples, vignettes, and tests
  remain compatible.

#### Enhancements for model diagnostics:

- **New function
  [`check_or()`](https://craig-parylo.github.io/plotor/reference/check_or.md)**
  ([\#62](https://github.com/craig-parylo/plotor/issues/62))

  Added an exported function,
  [`check_or()`](https://craig-parylo.github.io/plotor/reference/check_or.md),
  to provide users with detailed feedback on whether their logistic
  regression models meet underlying assumptions. Previously, detailed
  diagnostics were only accessible via undocumented internal functions.

- **Assumptions: check for sample size**
  ([\#41](https://github.com/craig-parylo/plotor/issues/41))

  Introduced a check for sufficient sample size, further improving
  diagnostics for logistic regression models.

These improvements make the package more robust to upstream changes in
dependencies and offer users more transparent and accessible model
validation tools.

## plotor 0.6.0

CRAN release: 2025-05-28

#### User-focussed changes

- **Summary OR tables**
  ([\#28](https://github.com/craig-parylo/plotor/issues/28))

  Introduced summary tables for odds ratios, making it easier to view
  and interpret results from your model.

- **Faster estimates of confidence intervals**
  ([\#53](https://github.com/craig-parylo/plotor/issues/53))

  Optional argument, `confint_fast_estimate`, for both
  [`plot_or()`](https://craig-parylo.github.io/plotor/reference/plot_or.md)
  and
  [`table_or()`](https://craig-parylo.github.io/plotor/reference/table_or.md)
  that allows for faster approximation of confidence intervals using
  [`stats::confint.default()`](https://rdrr.io/r/stats/confint.html).
  This can be helpful for large data sets where confidence intervals can
  take a long time to calculate for.

- **Improved validation of the confidence level**
  ([\#29](https://github.com/craig-parylo/plotor/issues/29))

  Enhanced how the package checks user input for confidence levels,
  reducing the risk of invalid values being used.

  This included enhanced checks in the internal function
  `validate_conf_level_inputs()` with enhanced error handling and user
  feedback ([\#31](https://github.com/craig-parylo/plotor/issues/31)).

- **Assumption checks**

  Started a suite of checks that assumptions for logistic regression are
  upheld. Implemented in this release:

  **Assumptions: check outcome is binary**
  ([\#42](https://github.com/craig-parylo/plotor/issues/42))

  Added logic to confirm the outcome variable is binary, as required for
  odds ratio calculations.

  **Assumptions: check for multicollinearity**
  ([\#43](https://github.com/craig-parylo/plotor/issues/43))

  Implemented checks to detect multicollinearity among predictors,
  helping users identify and address issues that could affect model
  validity.

  **Assumptions: check for separation**
  ([\#47](https://github.com/craig-parylo/plotor/issues/47))

  Added checks for separation in the data, which can cause estimation
  problems in logistic regression.

- **Updated README**

  Improved the README documentation, making it easier for users to get
  started and understand the package.

#### Developer-focussed changes

- **Test suite (Developer focus)**
  ([\#33](https://github.com/craig-parylo/plotor/issues/33),
  [\#37](https://github.com/craig-parylo/plotor/issues/37))

  Added and developed a suite of tests for ensuring code reliability and
  maintaining quality as the package evolves.

- **Bug fixes**

  Addressed and resolved warnings related to the {tidyselect} package,
  leading to cleaner output and better compatibility with the tidyverse
  ecosystem. ([\#34](https://github.com/craig-parylo/plotor/issues/34))

  Updated the way class descriptions are handled, consolidating them
  into single strings for consistency and clarity.
  ([\#50](https://github.com/craig-parylo/plotor/issues/50))

  Fixed ordering of terms and levels in
  [`table_or()`](https://craig-parylo.github.io/plotor/reference/table_or.md),
  so results are presented in a logical and expected sequence.
  ([\#54](https://github.com/craig-parylo/plotor/issues/54),
  [\#56](https://github.com/craig-parylo/plotor/issues/56))

For the full details, see the changelog:
<https://github.com/craig-parylo/plotor/compare/v0.5.2>…v0.6.0

## plotor 0.5.2

CRAN release: 2025-02-09

- [`plot_or()`](https://craig-parylo.github.io/plotor/reference/plot_or.md)
  now respects the order of covariates in the formula when plotting
  ([\#15](https://github.com/craig-parylo/plotor/issues/15)).

- [`plot_or()`](https://craig-parylo.github.io/plotor/reference/plot_or.md)
  handles missing information to avoid {ggplot2} related warning
  messages ([\#11](https://github.com/craig-parylo/plotor/issues/11)).

- [`plot_or()`](https://craig-parylo.github.io/plotor/reference/plot_or.md)
  accepts customised confidence limits, e.g. 99%, used when calculating
  the confidence intervals
  ([\#19](https://github.com/craig-parylo/plotor/issues/19)).

- [`plot_or()`](https://craig-parylo.github.io/plotor/reference/plot_or.md)
  conducts checks on inputs - ensuring the {glm} model is a logistic
  regression (family = ‘binomial’ and link = ‘logit’) and validates the
  confidence limit to be within the range 0.001 to 0.999
  ([\#22](https://github.com/craig-parylo/plotor/issues/22),
  [\#19](https://github.com/craig-parylo/plotor/issues/19)).

## plotor 0.5.1

CRAN release: 2024-09-27

## plotor 0.5.0

## plotor 0.1.0

- Initial CRAN submission.
