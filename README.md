
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tipr: R tools for tipping point sensitivity analyses

<!-- badges: start -->

[![R-CMD-check](https://github.com/LucyMcGowan/tipr/workflows/R-CMD-check/badge.svg)](https://github.com/LucyMcGowan/tipr/actions)
<!-- badges: end -->

**Authors:** [Lucy D’Agostino
McGowan](https://www.lucymcgowan.com/)<br/> **License:**
[MIT](https://opensource.org/licenses/MIT)

## Installation

Install the CRAN version

``` r
install.packages("tipr")
```

Or install the development version from GitHub:

``` r
# install.packages(devtools)
devtools::install_github("lucymcgowan/tipr")
```

``` r
library(tipr)
```

## Usage

After fitting your model, you can determine the unmeasured confounder
needed to tip your analysis. This unmeasured confounder is determined by
two quantities, the relationship between the exposure and the unmeasured
confounder (if the unmeasured confounder is continuous, this is
indicated with `exposure_confounder_effect`, if binary, with
`exposed_confounder_prev` and `unexposed_confounder_prev`), and the
relationship between the unmeasured confounder and outcome
`confounder_outcome_effect`. Using this 📦, we can fix one of these and
solve for the other. Alternatively, we can fix both and solve for `n`,
that is, how many unmeasured confounders of this magnitude would tip the
analysis.

In this example, a model was fit and the exposure-outcome relationship
was 1.5 (95% CI: 1.2, 1.8).

## Continuous unmeasured confounder example

We are interested in a continuous unmeasured confounder, so we will use
the `tip_with_continuous()` function. The package comes with an example
data set simulated for this scenario, `exdata_continuous`.

Let’s assume the relationship between the unmeasured confounder and
outcome is 1.5 (`confounder_outcome_effect = 1.5`), let’s solve for the
relationship between the exposure and unmeasured confounder needed to
tip the analysis (in this case, we are solving for
`exposure_confounder_effect`, the mean difference needed between the
exposed and unexposed).

``` r
tip(effect_observed = 1.2, confounder_outcome_effect = 1.5)
```

    ## The observed effect (1.2) WOULD be tipped by 1 unmeasured confounder
    ## with the following specifications:
    ##   * estimated difference in scaled means between the unmeasured confounder
    ##     in the exposed population and unexposed population: 0.45
    ##   * estimated relationship between the unmeasured confounder and the outcome: 1.5

    ## # A tibble: 1 × 5
    ##   effect_adjusted effect_observed exposure_confounder_effect confounder_outcome…
    ##             <dbl>           <dbl>                      <dbl>               <dbl>
    ## 1               1             1.2                      0.450                 1.5
    ## # … with 1 more variable: n_unmeasured_confounders <dbl>

A hypothetical unobserved continuous confounder that has a relationship
of 1.5 with the outcome would need a scaled mean difference between
exposure groups of `0.45` to tip this analysis at the 5% level,
rendering it inconclusive.

## Binary unmeasured confounder example

Now we are interested in the binary unmeasured confounder, so we will
use the `tip_with_binary()` function.

Let’s assume the unmeasured confounder is prevalent in 25% of the
exposed population (`exposed_confounder_prev = 0.25`) and in 10% of the
unexposed population (`unexposed_confounder_prev = 0.10`) – let’s solve
for the relationship between the unmeasured confounder and the outcome
needed to tip the analysis (`confounder_outcome_effect`).

``` r
tip_with_binary(effect_observed = 1.2, 
                exposed_confounder_prev = 0.25, 
                unexposed_confounder_prev = 0.10)
```

    ## The observed effect (1.2) WOULD be tipped by 1 unmeasured confounder
    ## with the following specifications:
    ##   * estimated prevalence of the unmeasured confounder in the exposed population: 0.25
    ##   * estimated prevalence of the unmeasured confounder in the unexposed population: 0.1
    ##   * estimated relationship between the unmeasured confounder and the outcome: 2.54

    ## # A tibble: 1 × 6
    ##   effect_adjusted effect_observed exposed_confounder_prev unexposed_confounder_…
    ##             <dbl>           <dbl>                   <dbl>                  <dbl>
    ## 1               1             1.2                    0.25                    0.1
    ## # … with 2 more variables: confounder_outcome_effect <dbl>,
    ## #   n_unmeasured_confounders <dbl>

A hypothetical unobserved binary confounder that is prevalent in 10% of
the unexposed population and 25% of the exposed population would need to
have a relationship with the outcome of 2.5 to tip this analysis at the
5% level, rendering it inconclusive.

## Many unmeasured confounders

Suppose we are concerned that there are many small, independent,
continuous, unmeasured confounders present.

``` r
tip(effect_observed = 1.2, 
    exposure_confounder_effect = 0.25, 
    confounder_outcome_effect = 1.05)
```

    ## The observed effect (1.2) WOULD be tipped by 15 unmeasured confounders
    ## with the following specifications:
    ##   * estimated difference in scaled means between the unmeasured confounder
    ##     in the exposed population and unexposed population: 0.25
    ##   * estimated relationship between the unmeasured confounder and the outcome: 1.05

    ## # A tibble: 1 × 5
    ##   effect_adjusted effect_observed exposure_confounder_effect confounder_outcome…
    ##             <dbl>           <dbl>                      <dbl>               <dbl>
    ## 1               1             1.2                       0.25                1.05
    ## # … with 1 more variable: n_unmeasured_confounders <dbl>

It would take about `15` independent standardized Normal unmeasured
confounders with a mean difference between exposure groups of 0.25 to
and a relationship with the outcome of 1.05 tip the observed analysis at
the 5% level, rendering it inconclusive.

## Integration with broom

These functions were created to easily integrate with models tidied
using the **broom** package. This is not *necessary* to use these
functions, but a nice feature if you choose to do so. Here is an example
of a logistic regression fit with `glm` and tidied with the `tidy`
function **broom** that can be directly fed into the `tip()` function.

``` r
if (requireNamespace("broom", quietly = TRUE) &&  requireNamespace("dplyr", quietly = TRUE)) {
   glm(am ~ mpg, data = mtcars, family = "binomial") %>%
    broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
    dplyr::filter(term == "mpg") %>%
    dplyr::pull(conf.low) %>%
    tip(confounder_outcome_effect = 2.5)
}
```

    ## The observed effect (1.13) WOULD be tipped by 1 unmeasured confounder
    ## with the following specifications:
    ##   * estimated difference in scaled means between the unmeasured confounder
    ##     in the exposed population and unexposed population: 0.13
    ##   * estimated relationship between the unmeasured confounder and the outcome: 2.5

    ## # A tibble: 1 × 5
    ##   effect_adjusted effect_observed exposure_confounder_effect confounder_outcome…
    ##             <dbl>           <dbl>                      <dbl>               <dbl>
    ## 1               1            1.13                      0.133                 2.5
    ## # … with 1 more variable: n_unmeasured_confounders <dbl>

## Code of Conduct

Please note that the tipr project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
