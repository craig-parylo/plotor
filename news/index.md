# Changelog

## plotor v1.0.0 (2026-02-14)

CRAN release: 2026-02-14

### Enhancements

- **Runtime benchmarking & prediction**  
  Benchmarks measuring plot / table runtime across dataset sizes and
  predictor mixes, plus a packaged regression model and lightweight
  wrapper `predict_process_time()` that returns an estimated runtime
  (ms) with uncertainty bounds. Benchmark outputs and the fitted model
  are stored as .rds assets for package use
  ([\#94](https://github.com/craig-parylo/plotor/pull/94))

- **Confirm before long runs**  
  `double_check_confint_fast_estimate()` prompts users before running
  slower confidence-interval computations when the predictored runtime
  exceeds the default threshold (60s). Reduces unexpected long
  operations in interactive sessions
  ([\#94](https://github.com/craig-parylo/plotor/pull/94))

- **Background processing with spinner**  
  Long-running work now runs in background R processes
  ([`callr::r_bg()`](https://callr.r-lib.org/reference/r_bg.html)),
  while the main process shows a [cli](https://cli.r-lib.org) spinner.
  Helpers include `get_summary_table_with_spinner()`,
  `check_assumptions_with_spinner()`, and `user_spinner()`. Spinners are
  suppressed in tests / CI
  ([\#96](https://github.com/craig-parylo/plotor/pull/96))
  ([\#97](https://github.com/craig-parylo/plotor/pull/97))

- **Influential-observation diagnostics**  
  New `assumption_no_extreme_values()` detects influential observations
  using Cook’s distance, leverage and standardised residuals with
  adaptive thresholds and conservative flagging. Test-data generators
  `get_df_influential()` and `get_lr_influential()` added
  ([\#98](https://github.com/craig-parylo/plotor/pull/98))

- **Reproducible README figures**  
  Script `tools/generate_readme_figures.R`, refreshed figures assets in
  `man/figures` and added Suggests dependencies for their reproduction
  (`here`, `webshot2`) so README builds are reproducible
  ([\#100](https://github.com/craig-parylo/plotor/pull/100))

### Changed

- **Runtime prediction integrated into UI**  
  [`plot_or()`](https://craig-parylo.github.io/plotor/reference/plot_or.md)
  and
  [`table_or()`](https://craig-parylo.github.io/plotor/reference/table_or.md)
  call `predict_process_time()` and prompt via the double-check helper
  when long runs are predicted. Benchmark code moved to `data-raw/`;
  serialised assets are shipped for app use (no re-benchmarking at
  runtime) ([\#94](https://github.com/craig-parylo/plotor/pull/94))

- **Safe background wrappers**  
  Wrapper functions run heavy work in background processes and return
  identical results while surfacing errors / warnings. Spinner display
  is controlled by `use_spinner()`
  ([\#96](https://github.com/craig-parylo/plotor/pull/96))

- **Namespace and tidy-eval hardening**  
  Explicit qualifications
  ([`utils::stack()`](https://rdrr.io/r/utils/stack.html),
  [`stats::predict()`](https://rdrr.io/r/stats/predict.html)), use of
  `.data$` pronoun and improved Roxygen for `use_spinner()` reduce R CMD
  check notes and improve compatibility with `dplyr` v1.2+. `callr`
  added to Imports
  ([\#97](https://github.com/craig-parylo/plotor/pull/97))

- **Stable transformations**  
  Replaced [`log1p()`](https://rdrr.io/r/base/Log.html) with
  [`asinh()`](https://rdrr.io/r/base/Hyperbolic.html) for
  continuous-predictor transforms to handle negative values and improve
  numerical stability
  ([\#98](https://github.com/craig-parylo/plotor/pull/98))

- **README rewrite**  
  README content simplified and focused; figures and generation workflow
  updated for reproducibility
  ([\#100](https://github.com/craig-parylo/plotor/pull/100))

### Fixed

- **Separation detection**  
  Fixed tidy-eval bugs, zero-count detection, empty-summary guards and
  NA-to_TRUE/FALSE issues in `assumption_no_separation_fast()` so
  separation is reliably detected
  ([\#96](https://github.com/craig-parylo/plotor/pull/96))

- **Spinner / test stability**  
  Spinners are disabled in non-interactive / test environments; minor
  test-data tweaks improve CI reliability
  ([\#97](https://github.com/craig-parylo/plotor/pull/97))

- **Assumption checks robustness**  
  Improved numerical stability, error handling and reporting across
  assumption checks to avoid spurious results
  ([\#96](https://github.com/craig-parylo/plotor/pull/96))

- **R CMD check issues**  
  Resolved undefined global variables and unqualified generic calls to
  clean up check output
  ([\#97](https://github.com/craig-parylo/plotor/pull/97))

### Notes / migration

- `callr` is now required (imported) for background processing
  ([\#97](https://github.com/craig-parylo/plotor/pull/97))

- Interactive users will see a spinner during long operations; spinners
  are suppressed in CI / tests
  ([\#96](https://github.com/craig-parylo/plotor/pull/96))

- No breaking API changes expected; function signatures remain
  compatible

## plotor v0.9.0 (2026-01-06) - GitHub release

### Enhancements

- **Support for ordered-factor predictors**  
  Formula parsing and model-summary utilities now recognise and
  correctly handle ordered factors
  ([\#91](https://github.com/craig-parylo/plotor/pull/91))

- **New helper**  
  `make_ordered_factors_compatible_with_broom()` aligns term names and
  labels with
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
  ([\#91](https://github.com/craig-parylo/plotor/pull/91))

- **Tests and examples**  
  Tests, example data and models covering ordered-factor predictors,
  including a truncated example that verifies the sample-size assumption
  warning ([\#91](https://github.com/craig-parylo/plotor/pull/91))

### Changes

- **Revert to gtExtras for CI mini-plots**  
  Replaced hard-coded copies with `gtExtras` code for
  confidence-interval plots produced by
  [`table_or()`](https://craig-parylo.github.io/plotor/reference/table_or.md)
  and require `gtExtras >= 0.6.1` to reduce duplication and use upstream
  fixes ([\#90](https://github.com/craig-parylo/plotor/pull/90))

- **Bump ggplot2 requirement**  
  Update dependency to `ggplot2 >= 4.0.0`
  ([\#90](https://github.com/craig-parylo/plotor/pull/90))

- **Refined predictor-type detection**  
  Refactored to return more detailed classes (e.g. “ordered factor”,
  “factor”, “numeric”) for improved downstream handling and displays
  ([\#91](https://github.com/craig-parylo/plotor/pull/91))

- **Version bump**  
  Bumped package version to 0.9.0 to support a GitHub release
  ([\#93](https://github.com/craig-parylo/plotor/pull/93))

### Fixed

- **Ordered-factor row counts**  
  Added dedicated row-count logic for ordered-factor levels to ensure
  correct summarisation and avoid miscounts in summaries and tables
  ([\#91](https://github.com/craig-parylo/plotor/pull/91))

### Notes / migration

- **Dependency updates required**  
  Users must have `gtExtras >= 0.6.1` and `ggplot2 >= 4.0.0` installed;
  older versions may cause plot failures
  ([\#90](https://github.com/craig-parylo/plotor/pull/90))

- **No external API changes**  
  Plotting and table functions should behave as before while preserving
  ordered-factor level ordering.
  ([\#90](https://github.com/craig-parylo/plotor/pull/90))([\#91](https://github.com/craig-parylo/plotor/pull/91))

## plotor v0.8.0 (2025-12-04)

CRAN release: 2025-12-08

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
  Documentation for `anonymise_count_values()` has been corrected: this
  internal function is no longer exported in the help files.  
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
