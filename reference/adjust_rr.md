# Adjust an observed risk ratio for a normally distributed confounder

Adjust an observed risk ratio for a normally distributed confounder

## Usage

``` r
adjust_rr(
  effect_observed,
  exposure_confounder_effect,
  confounder_outcome_effect,
  verbose = TRUE
)

adjust_rr_with_continuous(
  effect_observed,
  exposure_confounder_effect,
  confounder_outcome_effect,
  verbose = TRUE
)
```

## Arguments

- effect_observed:

  Numeric positive value. Observed exposure - outcome risk ratio. This
  can be the point estimate, lower confidence bound, or upper confidence
  bound.

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
adjust_rr(1.2, 0.5, 1.1)
#> ℹ The observed effect (RR: 1.2) is updated to RR: 1.14 by a confounder with the
#>   following specifications:
#> • estimated difference in scaled means: 0.5
#> • estimated relationship (RR) between the unmeasured confounder and the
#>   outcome: 1.1
#> # A tibble: 1 × 4
#>   rr_adjusted rr_observed exposure_confounder_effect confounder_outcome_effect
#>         <dbl>       <dbl>                      <dbl>                     <dbl>
#> 1        1.14         1.2                        0.5                       1.1
```
