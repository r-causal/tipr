# Adjust an observed regression coefficient for a normally distributed confounder

Adjust an observed regression coefficient for a normally distributed
confounder

## Usage

``` r
adjust_coef(
  effect_observed,
  exposure_confounder_effect,
  confounder_outcome_effect,
  verbose = getOption("tipr.verbose", TRUE)
)

adjust_coef_with_continuous(
  effect_observed,
  exposure_confounder_effect,
  confounder_outcome_effect,
  verbose = getOption("tipr.verbose", TRUE)
)
```

## Arguments

- effect_observed:

  Numeric. Observed exposure - outcome effect from a regression model.
  This can be the beta coefficient, the lower confidence bound of the
  beta coefficient, or the upper confidence bound of the beta
  coefficient.

- exposure_confounder_effect:

  Numeric. Estimated difference in scaled means between the unmeasured
  confounder in the exposed population and unexposed population

- confounder_outcome_effect:

  Numeric. Estimated relationship between the unmeasured confounder and
  the outcome.

- verbose:

  Logical. Indicates whether to print informative message. Default:
  `TRUE`

## Value

Data frame.

## Examples

``` r
## Update an observed coefficient of 0.5 with an unmeasured confounder
## with a difference in scaled means between exposure groups of 0.2
## and coefficient of 0.3
adjust_coef(0.5, 0.2, 0.3)
#> ℹ The observed effect (0.5) is updated to 0.44 by a confounder with the
#>   following specifications:
#> • estimated difference in scaled means: 0.2
#> • estimated relationship between the unmeasured confounder and the outcome: 0.3
#> # A tibble: 1 × 4
#>   effect_adjusted effect_observed exposure_confounder_e…¹ confounder_outcome_e…²
#>             <dbl>           <dbl>                   <dbl>                  <dbl>
#> 1            0.44             0.5                     0.2                    0.3
#> # ℹ abbreviated names: ¹​exposure_confounder_effect, ²​confounder_outcome_effect
```
