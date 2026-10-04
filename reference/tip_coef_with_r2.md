# Tip a regression coefficient using the partial R2 for an unmeasured confounder-exposure relationship and unmeasured confounder- outcome relationship

Choose one of the following, and the other will be estimated:

- `confounder_exposure_r2`

- `confounder_outcome_r2`

## Usage

``` r
tip_coef_with_r2(
  effect_observed,
  se,
  df,
  confounder_exposure_r2 = NULL,
  confounder_outcome_r2 = NULL,
  verbose = getOption("tipr.verbose", TRUE),
  alpha = 0.05,
  tip_bound = FALSE,
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

- tip_bound:

  Do you want to tip at the bound? Default = `FALSE`, will tip at the
  point estimate

- ...:

  Optional arguments passed to the
  [`sensemakr::adjusted_estimate()`](https://rdrr.io/pkg/sensemakr/man/adjusted_estimate.html)
  function.

## Value

A data frame.

## Examples

``` r
tip_coef_with_r2(0.5, 0.1, 102, 0.5)
#> # A tibble: 1 × 10
#>   effect_adjusted lb_adjusted ub_adjusted effect_observed lb_observed
#>             <dbl>       <dbl>       <dbl>           <dbl>       <dbl>
#> 1        5.55e-17      -0.245       0.245             0.5       0.302
#> # ℹ 5 more variables: ub_observed <dbl>, se_observed <dbl>, df_observed <dbl>,
#> #   confounder_exposure_r2 <dbl>, confounder_outcome_r2 <dbl>
```
