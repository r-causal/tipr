# Adjust an observed coefficient from a regression model with a binary confounder

Adjust an observed coefficient from a regression model with a binary
confounder

## Usage

``` r
adjust_coef_with_binary(
  effect_observed,
  exposed_confounder_prev,
  unexposed_confounder_prev,
  confounder_outcome_effect,
  loglinear = FALSE,
  verbose = getOption("tipr.verbose", TRUE)
)
```

## Arguments

- effect_observed:

  Numeric. Observed exposure - outcome effect from a loglinear model.
  This can be the beta coefficient, the lower confidence bound of the
  beta coefficient, or the upper confidence bound of the beta
  coefficient.

- exposed_confounder_prev:

  Numeric between 0 and 1. Estimated prevalence of the unmeasured
  confounder in the exposed population

- unexposed_confounder_prev:

  Numeric between 0 and 1. Estimated prevalence of the unmeasured
  confounder in the unexposed population

- confounder_outcome_effect:

  Numeric. Estimated relationship between the unmeasured confounder and
  the outcome.

- loglinear:

  Logical. Calculate the adjusted coefficient from a loglinear model
  instead of a linear model (the default). When `loglinear = FALSE`,
  `adjust_coef_with_binary()` is equivalent to
  [`adjust_coef()`](adjust_coef.md) where `exposure_confounder_effect`
  is the difference in prevalences.

- verbose:

  Logical. Indicates whether to print informative message. Default:
  `TRUE`

## Value

Data frame.

## Examples

``` r
adjust_coef_with_binary(1.1, 0.5, 0.3, 1.3)
#> ℹ The observed effect (1.1) is updated to 0.84 by a confounder with the
#>   following specifications:
#> • estimated prevalence of the unmeasured confounder in the exposed population:
#>   0.5
#> • estimated prevalence of the unmeasured confounder in the unexposed
#>   population: 0.3
#> • estimated relationship between the unmeasured confounder and the outcome: 1.3
#> # A tibble: 1 × 4
#>   effect_adjusted effect_observed exposure_confounder_e…¹ confounder_outcome_e…²
#>             <dbl>           <dbl>                   <dbl>                  <dbl>
#> 1            0.84             1.1                     0.2                    1.3
#> # ℹ abbreviated names: ¹​exposure_confounder_effect, ²​confounder_outcome_effect
```
