# Adjust an observed risk ratio with a binary confounder

Adjust an observed risk ratio with a binary confounder

## Usage

``` r
adjust_rr_with_binary(
  effect_observed,
  exposed_confounder_prev,
  unexposed_confounder_prev,
  confounder_outcome_effect,
  verbose = getOption("tipr.verbose", TRUE)
)
```

## Arguments

- effect_observed:

  Numeric positive value. Observed exposure - outcome risk ratio. This
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

## Value

Data frame.

## Examples

``` r
adjust_rr_with_binary(1.1, 0.5, 0.3, 1.3)
#> The observed effect (1.1) is updated to 1.04 by a confounder with the following
#> specifications:
#> • estimated prevalence of the unmeasured confounder in the exposed population:
#>   0.5
#> • estimated prevalence of the unmeasured confounder in the unexposed
#>   population: 0.3
#> • estimated relationship between the unmeasured confounder and the outcome: 1.3
#> # A tibble: 1 × 5
#>   rr_adjusted rr_observed exposed_confounder_prev unexposed_confounder_prev
#>         <dbl>       <dbl>                   <dbl>                     <dbl>
#> 1        1.04         1.1                     0.5                       0.3
#> # ℹ 1 more variable: confounder_outcome_effect <dbl>
```
