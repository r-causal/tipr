# Tip a result with a binary confounder.

Choose two of the following three to specify, and the third will be
estimated:

- `exposed_confounder_prev`

- `unexposed_confounder_prev`

- `confounder_outcome_effect`

Alternatively, specify all three and the function will return the number
of unmeasured confounders specified needed to tip the analysis.

## Usage

``` r
tip_with_binary(
  effect_observed,
  exposed_confounder_prev = NULL,
  unexposed_confounder_prev = NULL,
  confounder_outcome_effect = NULL,
  verbose = getOption("tipr.verbose", TRUE),
  correction_factor = "none"
)

tip_b(
  effect_observed,
  exposed_confounder_prev = NULL,
  unexposed_confounder_prev = NULL,
  confounder_outcome_effect = NULL,
  verbose = getOption("tipr.verbose", TRUE),
  correction_factor = "none"
)
```

## Arguments

- effect_observed:

  Numeric positive value. Observed exposure - outcome effect (assumed to
  be the exponentiated coefficient, so a risk ratio, odds ratio, or
  hazard ratio). This can be the point estimate, lower confidence bound,
  or upper confidence bound.

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

- correction_factor:

  Character string. Options are "none", "hr", "or". For common outcomes
  (\>15%), the odds ratio or hazard ratio is not a good estimate for the
  risk ratio. In these cases, we can apply a correction factor. If you
  are supplying a hazard ratio for a common outcome, set this to "hr";
  if you are supplying an odds ratio for a common outcome, set this to
  "or"; if you are supplying a risk ratio or your outcome is rare, set
  this to "none" (default).

## Details

`tip_b()` is an alias for `tip_with_binary()`.

## Examples

``` r
## to estimate the relationship between an unmeasured confounder and outcome
## needed to tip analysis
tip_with_binary(1.2, exposed_confounder_prev = 0.5, unexposed_confounder_prev = 0)
#> ℹ The observed effect (1.2) WOULD be tipped by 1 unmeasured confounder with the
#>   following specifications:
#> • estimated prevalence of the unmeasured confounder in the exposed population:
#>   0.5
#> • estimated prevalence of the unmeasured confounder in the unexposed
#>   population: 0
#> • estimated relationship between the unmeasured confounder and the outcome: 1.4
#> # A tibble: 1 × 6
#>   effect_adjusted effect_observed exposed_confounder_prev unexposed_confounder…¹
#>             <dbl>           <dbl>                   <dbl>                  <dbl>
#> 1               1             1.2                     0.5                      0
#> # ℹ abbreviated name: ¹​unexposed_confounder_prev
#> # ℹ 2 more variables: confounder_outcome_effect <dbl>,
#> #   n_unmeasured_confounders <dbl>

## to estimate the number of unmeasured confounders specified needed to tip
## the analysis
tip_with_binary(1.2,
  exposed_confounder_prev = 0.5,
  unexposed_confounder_prev = 0,
  confounder_outcome_effect = 1.1)
#> ℹ The observed effect (1.2) WOULD be tipped by 4 unmeasured confounders with
#>   the following specifications:
#> • estimated prevalence of the unmeasured confounder in the exposed population:
#>   0.5
#> • estimated prevalence of the unmeasured confounder in the unexposed
#>   population: 0
#> • estimated relationship between the unmeasured confounder and the outcome: 1.1
#> # A tibble: 1 × 6
#>   effect_adjusted effect_observed exposed_confounder_prev unexposed_confounder…¹
#>             <dbl>           <dbl>                   <dbl>                  <dbl>
#> 1               1             1.2                     0.5                      0
#> # ℹ abbreviated name: ¹​unexposed_confounder_prev
#> # ℹ 2 more variables: confounder_outcome_effect <dbl>,
#> #   n_unmeasured_confounders <dbl>

## Example with broom
if (requireNamespace("broom", quietly = TRUE) &&
    requireNamespace("dplyr", quietly = TRUE)) {
  glm(am ~ mpg, data = mtcars, family = "binomial") %>%
   broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
   dplyr::filter(term == "mpg") %>%
   dplyr::pull(conf.low) %>%
   tip_with_binary(exposed_confounder_prev = 1, confounder_outcome_effect = 1.15)
}
#> ℹ The observed effect (1.13) WOULD be tipped by 1 unmeasured confounder with
#>   the following specifications:
#> • estimated prevalence of the unmeasured confounder in the exposed population:
#>   1
#> • estimated prevalence of the unmeasured confounder in the unexposed
#>   population: 0.12
#> • estimated relationship between the unmeasured confounder and the outcome:
#>   1.15
#> # A tibble: 1 × 6
#>   effect_adjusted effect_observed exposed_confounder_prev unexposed_confounder…¹
#>             <dbl>           <dbl>                   <dbl>                  <dbl>
#> 1               1            1.13                       1                  0.119
#> # ℹ abbreviated name: ¹​unexposed_confounder_prev
#> # ℹ 2 more variables: confounder_outcome_effect <dbl>,
#> #   n_unmeasured_confounders <dbl>
```
