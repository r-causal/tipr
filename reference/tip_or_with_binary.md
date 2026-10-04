# Tip an observed odds ratio with a binary confounder.

Choose two of the following three to specify, and the third will be
estimated:

- `exposed_confounder_prev`

- `unexposed_confounder_prev`

- `confounder_outcome_effect`

Alternatively, specify all three and the function will return the number
of unmeasured confounders specified needed to tip the analysis.

## Usage

``` r
tip_or_with_binary(
  effect_observed,
  exposed_confounder_prev = NULL,
  unexposed_confounder_prev = NULL,
  confounder_outcome_effect = NULL,
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
tip_or_with_binary(0.9, 0.9, 0.1)
#> ℹ The observed effect (0.9) WOULD be tipped by 1 unmeasured confounder with the
#>   following specifications:
#> • estimated prevalence of the unmeasured confounder in the exposed population:
#>   0.9
#> • estimated prevalence of the unmeasured confounder in the unexposed
#>   population: 0.1
#> • estimated relationship between the unmeasured confounder and the outcome:
#>   0.88
#> # A tibble: 1 × 6
#>   effect_adjusted effect_observed exposed_confounder_prev unexposed_confounder…¹
#>             <dbl>           <dbl>                   <dbl>                  <dbl>
#> 1               1             0.9                     0.9                    0.1
#> # ℹ abbreviated name: ¹​unexposed_confounder_prev
#> # ℹ 2 more variables: confounder_outcome_effect <dbl>,
#> #   n_unmeasured_confounders <dbl>
```
