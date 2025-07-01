# plotor 0.7.0

### Compatibility and testing fixes:

* **Test suite** (#68)
  
  Addressed issues where snapshot tests failed depending on the installed 
  version of ggplot2. Snapshots produced by different ggplot2 versions were 
  causing test failures, especially with `vdiffr::expect_doppelganger()`. 
  
  The temporary solution was to suspend these visual comparison tests to avoid 
  unnecessary failures for users not on the latest ggplot2.

* **Readiness for upcoming {ggplot2} release** (#65) 
  
  Investigated and resolved failures with the upcoming major release of ggplot2, 
  ensuring that the package’s examples, vignettes, and tests remain compatible.

### Enhancements for model diagnostics:

* **New function `check_or()`** (#62) 

  Added an exported function, `check_or()`, to provide users with detailed 
  feedback on whether their logistic regression models meet underlying 
  assumptions. Previously, detailed diagnostics were only accessible via 
  undocumented internal functions.

* **Assumptions: check for sample size** (#41)
  
  Introduced a check for sufficient sample size, further improving diagnostics 
  for logistic regression models.

These improvements make the package more robust to upstream changes in 
dependencies and offer users more transparent and accessible model validation 
tools.


# plotor 0.6.0

### User-focussed changes

* **Summary OR tables** (#28)
  
  Introduced summary tables for odds ratios, making it easier to view and
  interpret results from your model.

* **Faster estimates of confidence intervals** (#53)
  
  Optional argument, `confint_fast_estimate`,  for both `plot_or()` and 
  `table_or()` that allows for faster approximation of confidence intervals
  using `stats::confint.default()`. This can be helpful for large data sets
  where confidence intervals can take a long time to calculate for.

* **Improved validation of the confidence level** (#29)

  Enhanced how the package checks user input for confidence levels, reducing the
  risk of invalid values being used. 

  This included enhanced checks in the internal function 
  `validate_conf_level_inputs()` with enhanced error handling and user feedback 
  (#31).

* **Assumption checks**
  
  Started a suite of checks that assumptions for logistic regression are upheld.
  Implemented in this release:
  
  **Assumptions: check outcome is binary** (#42)
  
  Added logic to confirm the outcome variable is binary, as required for odds
  ratio calculations.
  
  **Assumptions: check for multicollinearity** (#43)
  
  Implemented checks to detect multicollinearity among predictors, helping users
  identify and address issues that could affect model validity.
  
  **Assumptions: check for separation** (#47)
  
  Added checks for separation in the data, which can cause estimation problems
  in logistic regression.

* **Updated README**
  
  Improved the README documentation, making it easier for users to get started
  and understand the package.

### Developer-focussed changes

* **Test suite (Developer focus)** (#33, #37)

  Added and developed a suite of tests for ensuring code reliability and 
  maintaining quality as the package evolves.

* **Bug fixes**

  Addressed and resolved warnings related to the {tidyselect} package, leading 
  to cleaner output and better compatibility with the tidyverse ecosystem. (#34)
  
  Updated the way class descriptions are handled, consolidating them into single
  strings for consistency and clarity. (#50)
  
  Fixed ordering of terms and levels in `table_or()`, so results are presented
  in a logical and expected sequence. (#54, #56)


For the full details, see the changelog: https://github.com/craig-parylo/plotor/compare/v0.5.2...v0.6.0

# plotor 0.5.2

* `plot_or()` now respects the order of covariates in the formula when plotting 
(#15).

* `plot_or()` handles missing information to avoid {ggplot2} related warning 
messages (#11).

* `plot_or()` accepts customised confidence limits, e.g. 99%, used when 
calculating the confidence intervals (#19).

* `plot_or()` conducts checks on inputs - ensuring the {glm} model is 
a logistic regression (family = 'binomial' and link = 'logit') and validates the
confidence limit to be within the range 0.001 to 0.999 (#22, #19).

# plotor 0.5.1

# plotor 0.5.0

# plotor 0.1.0

* Initial CRAN submission.
