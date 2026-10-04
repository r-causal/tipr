# Adjust an observed hazard ratio with a binary confounder

Adjust an observed hazard ratio with a binary confounder

## Usage

``` r
adjust_hr_with_binary(
  effect_observed,
  exposed_confounder_prev,
  unexposed_confounder_prev,
  confounder_outcome_effect,
  verbose = getOption("tipr.verbose", TRUE),
  hr_correction = FALSE
)
```

## Arguments

- effect_observed:

  Numeric positive value. Observed exposure - outcome hazard ratio. This
  can be the point estimate, lower confidence bound, or upper confidence
  bound.

- exposed_confounder_prev:

  Numeric between 0 and 1. Estimated prevalence of the unmeasured
  confounder in the exposed population

- unexposed_confounder_prev:

  Numeric between 0 and 1. Estimated prevalence of the unmeasured
  confounder in the unexposed population

- confounder_outcome_effect:

  Numeric positive value. Estimated relationship between the unmeasured
  confounder and the outcome

- verbose:

  Logical. Indicates whether to print informative message. Default:
  `TRUE`

- hr_correction:

  Logical. Indicates whether to use a correction factor. The methods
  used for this function are based on risk ratios. For rare outcomes, a
  hazard ratio approximates a risk ratio. For common outcomes, a
  correction factor is needed. If you have a common outcome (\>15%), set
  this to `TRUE`. Default: `FALSE`.

## Value

Data frame.

## Examples

``` r
adjust_hr_with_binary(0.8, 0.1, 0.5, 1.8)
#> ℹ The observed effect (HR: 0.8) is updated to HR: 1.04 by a confounder with the
#>   following specifications:
#> • estimated prevalence of the unmeasured confounder in the exposed population:
#>   0.1
#> • estimated prevalence of the unmeasured confounder in the unexposed
#>   population: 0.5
#> • estimated relationship between the unmeasured confounder and the outcome: 1.8
#> # A tibble: 1 × 5
#>   hr_adjusted hr_observed exposed_confounder_prev unexposed_confounder_prev
#>         <dbl>       <dbl>                   <dbl>                     <dbl>
#> 1        1.04         0.8                     0.1                       0.5
#> # ℹ 1 more variable: confounder_outcome_effect <dbl>
```
