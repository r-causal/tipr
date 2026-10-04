# Robustness value

This function wraps the
[`sensemakr::robustness_value()`](https://rdrr.io/pkg/sensemakr/man/robustness_value.html)
function

## Usage

``` r
r_value(effect_observed, se, df, ...)
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

- ...:

  Optional arguments passed to the
  [`sensemakr::robustness_value()`](https://rdrr.io/pkg/sensemakr/man/robustness_value.html)
  function.

## Value

Numeric. Robustness value

## References

Carlos Cinelli, Jeremy Ferwerda and Chad Hazlett (2021). sensemakr:
Sensitivity Analysis Tools for Regression Models. R package version
0.1.4. https://CRAN.R-project.org/package=sensemakr

## Examples

``` r
r_value(0.5, 0.1, 102)
#> [1] 0.2566563
```
