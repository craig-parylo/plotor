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
