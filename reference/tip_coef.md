# Tip a linear model coefficient with a continuous confounder.

choose one of the following, and the other will be estimated:

- `exposure_confounder_effect`

- `confounder_outcome_effect`

## Usage

``` r
tip_coef(
  effect_observed,
  exposure_confounder_effect = NULL,
  confounder_outcome_effect = NULL,
  verbose = getOption("tipr.verbose", TRUE)
)

tip_coef_with_continuous(
  effect_observed,
  exposure_confounder_effect = NULL,
  confounder_outcome_effect = NULL,
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

  Numeric. Estimated scaled mean difference between the unmeasured
  confounder in the exposed population and unexposed population

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
## to estimate the relationship between an unmeasured confounder and outcome
## needed to tip analysis
tip_coef(1.2, exposure_confounder_effect = -2)
#> ℹ The observed effect (1.2) WOULD be tipped by 1 unmeasured confounder with the
#>   following specifications:
#> • estimated difference in scaled means between the unmeasured confounder in the
#>   exposed population and unexposed population: -2
#> • estimated relationship between the unmeasured confounder and the outcome:
#>   -0.6
#> # A tibble: 1 × 4
#>   effect_observed exposure_confounder_effect confounder_outcome_effect
#>             <dbl>                      <dbl>                     <dbl>
#> 1             1.2                         -2                      -0.6
#> # ℹ 1 more variable: n_unmeasured_confounders <dbl>

## to estimate the number of unmeasured confounders specified needed to tip
## the analysis
tip_coef(1.2, exposure_confounder_effect = -2, confounder_outcome_effect = -0.05)
#> ℹ The observed effect (1.2) WOULD be tipped by 12 unmeasured confounders with
#>   the following specifications:
#> • estimated difference in scaled means between the unmeasured confounder in the
#>   exposed population and unexposed population: -2
#> • estimated relationship between the unmeasured confounder and the outcome:
#>   -0.05
#> # A tibble: 1 × 4
#>   effect_observed exposure_confounder_effect confounder_outcome_effect
#>             <dbl>                      <dbl>                     <dbl>
#> 1             1.2                         -2                     -0.05
#> # ℹ 1 more variable: n_unmeasured_confounders <dbl>

## Example with broom
if (requireNamespace("broom", quietly = TRUE) &&
    requireNamespace("dplyr", quietly = TRUE)) {
  lm(wt ~ mpg, data = mtcars) %>%
   broom::tidy(conf.int = TRUE) %>%
   dplyr::filter(term == "mpg") %>%
   dplyr::pull(conf.low) %>%
   tip_coef(confounder_outcome_effect = 2.5)
}
#> ℹ The observed effect (-0.17) WOULD be tipped by 1 unmeasured confounder with
#>   the following specifications:
#> • estimated difference in scaled means between the unmeasured confounder in the
#>   exposed population and unexposed population: -0.07
#> • estimated relationship between the unmeasured confounder and the outcome: 2.5
#> # A tibble: 1 × 4
#>   effect_observed exposure_confounder_effect confounder_outcome_effect
#>             <dbl>                      <dbl>                     <dbl>
#> 1          -0.171                    -0.0684                       2.5
#> # ℹ 1 more variable: n_unmeasured_confounders <dbl>
```
