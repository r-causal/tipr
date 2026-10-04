# Tip an observed risk ratio with a binary confounder.

Choose two of the following three to specify, and the third will be
estimated:

- `exposed_confounder_prev`

- `unexposed_confounder_prev`

- `confounder_outcome_effect`

Alternatively, specify all three and the function will return the number
of unmeasured confounders specified needed to tip the analysis.

## Usage

``` r
tip_rr_with_binary(
  effect_observed,
  exposed_confounder_prev = NULL,
  unexposed_confounder_prev = NULL,
  confounder_outcome_effect = NULL,
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
