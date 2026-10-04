# Adjust an observed odds ratio for a normally distributed confounder

Adjust an observed odds ratio for a normally distributed confounder

## Usage

``` r
adjust_or(
  effect_observed,
  exposure_confounder_effect,
  confounder_outcome_effect,
  verbose = getOption("tipr.verbose", TRUE),
  or_correction = FALSE
)

adjust_or_with_continuous(
  effect_observed,
  exposure_confounder_effect,
  confounder_outcome_effect,
  verbose = getOption("tipr.verbose", TRUE),
  or_correction = FALSE
)
```

## Arguments

- effect_observed:

  Numeric positive value. Observed exposure - outcome odds ratio. This
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

- or_correction:

  Logical. Indicates whether to use a correction factor. The methods
  used for this function are based on risk ratios. For rare outcomes, an
  odds ratio approximates a risk ratio. For common outcomes, a
  correction factor is needed. If you have a common outcome (\>15%), set
  this to `TRUE`. Default: `FALSE`.

## Value

Data frame.

## Examples

``` r
adjust_or(1.2, 0.9, 1.3)
#> ℹ The observed effect (OR: 1.2) is updated to OR: 0.95 by a confounder with the
#>   following specifications:
#> • estimated difference in scaled means: 0.9
#> • estimated relationship (OR) between the unmeasured confounder and the
#>   outcome: 1.3
#> # A tibble: 1 × 4
#>   or_adjusted or_observed exposure_confounder_effect confounder_outcome_effect
#>         <dbl>       <dbl>                      <dbl>                     <dbl>
#> 1       0.948         1.2                        0.9                       1.3
```
