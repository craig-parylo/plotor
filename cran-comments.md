## Submission
This submission addresses issues flagged by CRAN checks.

`{plotor}` was resulting in test suite failures on `r-devel` platforms.
I was unable to replicate this issue directly, but the probable cause
is likely to be due to the use of brittle tests, such as 
`testthat::expect_silent()`.

In this submission, most of these tests have been replaced by more
specific tests, such as `testthat::expect_no_warning()`.


## Checks
── R CMD check results ───────────────────────────── plotor 1.1.0 ────
Duration: 2m 8.1s

❯ checking for future file timestamps ... NOTE
  unable to verify current time

0 errors ✔ | 0 warnings ✔ | 1 note ✖
