# Tip an observed odds ratio with a normally distributed confounder.

choose one of the following, and the other will be estimated:

- `exposure_confounder_effect`

- `confounder_outcome_effect`

## Usage

``` r
tip_or(
  effect_observed,
  exposure_confounder_effect = NULL,
  confounder_outcome_effect = NULL,
  verbose = getOption("tipr.verbose", TRUE),
  or_correction = FALSE
)

tip_or_with_continuous(
  effect_observed,
  exposure_confounder_effect = NULL,
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

- exposure_confounder_effect:

  Numeric. Estimated difference in scaled means between the unmeasured
  confounder in the exposed population and unexposed population

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
## to estimate the relationship between an unmeasured confounder and outcome
## needed to tip analysis
tip_or(1.2, exposure_confounder_effect = -2)
#> ℹ The observed effect (1.2) WOULD be tipped by 1 unmeasured confounder with the
#>   following specifications:
#> • estimated difference in scaled means between the unmeasured confounder in the
#>   exposed population and unexposed population: -2
#> • estimated relationship between the unmeasured confounder and the outcome:
#>   0.91
#> # A tibble: 1 × 5
#>   effect_adjusted effect_observed exposure_confounder_e…¹ confounder_outcome_e…²
#>             <dbl>           <dbl>                   <dbl>                  <dbl>
#> 1               1             1.2                      -2                  0.913
#> # ℹ abbreviated names: ¹​exposure_confounder_effect, ²​confounder_outcome_effect
#> # ℹ 1 more variable: n_unmeasured_confounders <dbl>

## to estimate the number of unmeasured confounders specified needed to tip
## the analysis
tip_or(1.2, exposure_confounder_effect = -2, confounder_outcome_effect = .99)
#> ℹ The observed effect (1.2) WOULD be tipped by 9 unmeasured confounders with
#>   the following specifications:
#> • estimated difference in scaled means between the unmeasured confounder in the
#>   exposed population and unexposed population: -2
#> • estimated relationship between the unmeasured confounder and the outcome:
#>   0.99
#> # A tibble: 1 × 5
#>   effect_adjusted effect_observed exposure_confounder_e…¹ confounder_outcome_e…²
#>             <dbl>           <dbl>                   <dbl>                  <dbl>
#> 1               1             1.2                      -2                   0.99
#> # ℹ abbreviated names: ¹​exposure_confounder_effect, ²​confounder_outcome_effect
#> # ℹ 1 more variable: n_unmeasured_confounders <dbl>

## Example with broom
if (requireNamespace("broom", quietly = TRUE) &&
    requireNamespace("dplyr", quietly = TRUE)) {
  glm(am ~ mpg, data = mtcars, family = "binomial") %>%
   broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
   dplyr::filter(term == "mpg") %>%
   dplyr::pull(conf.low) %>%
   tip_or(confounder_outcome_effect = 2.5, or_correction = TRUE)
}
#> ℹ The observed effect (1.06) WOULD be tipped by 1 unmeasured confounder with
#>   the following specifications:
#> • estimated difference in scaled means between the unmeasured confounder in the
#>   exposed population and unexposed population: 0.13
#> • estimated relationship between the unmeasured confounder and the outcome:
#>   1.58
#> ℹ You opted to use the odds ratio correction to convert your odds ratios to
#>   approximate risk ratios. This is a good idea if the outcome is common (>15%).
#> # A tibble: 1 × 5
#>   effect_adjusted effect_observed exposure_confounder_e…¹ confounder_outcome_e…²
#>             <dbl>           <dbl>                   <dbl>                  <dbl>
#> 1               1            1.06                   0.133                   1.58
#> # ℹ abbreviated names: ¹​exposure_confounder_effect, ²​confounder_outcome_effect
#> # ℹ 1 more variable: n_unmeasured_confounders <dbl>
```
