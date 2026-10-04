# Adjust a regression coefficient using the partial R2 for an unmeasured confounder-exposure relationship and unmeasured confounder- outcome relationship

This function wraps the
[`sensemakr::adjusted_estimate()`](https://rdrr.io/pkg/sensemakr/man/adjusted_estimate.html)
and
[`sensemakr::adjusted_se()`](https://rdrr.io/pkg/sensemakr/man/adjusted_estimate.html)
functions.

## Usage

``` r
adjust_coef_with_r2(
  effect_observed,
  se,
  df,
  confounder_exposure_r2,
  confounder_outcome_r2,
  verbose = getOption("tipr.verbose", TRUE),
  alpha = 0.05,
  ...
)
```

## Arguments

- effect_observed:

  Numeric. Observed exposure - outcome effect from a regression model.
  This is the point estimate (beta coefficient)

- se:

  Numeric. Standard error of the `effect_observed` in the previous
  parameter.

- df:

  Numeric positive value. Residual degrees of freedom for the model used
  to estimate the observed exposure - outcome effect. This is the total
  number of observations minus the number of parameters estimated in
  your model. Often for models estimated with an intercept this is N -
  k - 1 where k is the number of predictors in the model.

- confounder_exposure_r2:

  Numeric value between 0 and 1. The assumed partial R2 of the
  unobserved confounder with the exposure given the measured covariates.

- confounder_outcome_r2:

  Numeric value between 0 and 1. The assumed partial R2 of the
  unobserved confounder with the outcome given the exposure and the
  measured covariates.

- verbose:

  Logical. Indicates whether to print informative message. Default:
  `TRUE`

- alpha:

  Significance level. Default = `0.05`.

- ...:

  Optional arguments passed to the
  [`sensemakr::adjusted_estimate()`](https://rdrr.io/pkg/sensemakr/man/adjusted_estimate.html)
  function.

## Value

A data frame.

## References

Carlos Cinelli, Jeremy Ferwerda and Chad Hazlett (2021). sensemakr:
Sensitivity Analysis Tools for Regression Models. R package version
0.1.4. https://CRAN.R-project.org/package=sensemakr

## Examples

``` r
adjust_coef_with_r2(0.5, 0.1, 102, 0.05, 0.1)
#> # A tibble: 1 × 10
#>   effect_adjusted lb_adjusted ub_adjusted effect_observed lb_observed
#>             <dbl>       <dbl>       <dbl>           <dbl>       <dbl>
#> 1           0.427       0.233       0.621             0.5       0.302
#> # ℹ 5 more variables: ub_observed <dbl>, se_observed <dbl>, df_observed <dbl>,
#> #   confounder_exposure_r2 <dbl>, confounder_outcome_r2 <dbl>
```
