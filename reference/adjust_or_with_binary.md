# Adjust an observed odds ratio with a binary confounder

Adjust an observed odds ratio with a binary confounder

## Usage

``` r
adjust_or_with_binary(
  effect_observed,
  exposed_confounder_prev,
  unexposed_confounder_prev,
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
adjust_or_with_binary(3, 1, 0, 3)
#> • The observed effect (OR: 3) is updated to OR: 1 by a confounder with the
#>   following specifications:
#> • estimated prevalence of the unmeasured confounder in the exposed population:
#>   1
#> • estimated prevalence of the unmeasured confounder in the unexposed
#>   population: 0
#> • estimated relationship between the unmeasured confounder and the outcome: 3
#> # A tibble: 1 × 5
#>   or_adjusted or_observed exposed_confounder_prev unexposed_confounder_prev
#>         <dbl>       <dbl>                   <dbl>                     <dbl>
#> 1           1           3                       1                         0
#> # ℹ 1 more variable: confounder_outcome_effect <dbl>
adjust_or_with_binary(3, 1, 0, 3, or_correction = TRUE)
#> • The observed effect (RR: 1.73) is updated to RR: 1 by a confounder with the
#>   following specifications:
#> • estimated prevalence of the unmeasured confounder in the exposed population:
#>   1
#> • estimated prevalence of the unmeasured confounder in the unexposed
#>   population: 0
#> • estimated relationship between the unmeasured confounder and the outcome:
#>   1.73
#> ℹ You opted to use the odds ratio correction to convert your odds ratios to
#>   approximate risk ratios. This is a good idea if the outcome is common (>15%)
#> # A tibble: 1 × 5
#>   rr_adjusted rr_observed exposed_confounder_prev unexposed_confounder_prev
#>         <dbl>       <dbl>                   <dbl>                     <dbl>
#> 1           1        1.73                       1                         0
#> # ℹ 1 more variable: confounder_outcome_effect <dbl>
```
