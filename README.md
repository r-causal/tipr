
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tipr: R tools for tipping point sensitivity analyses <img src="man/figures/logo.png" align="right" height="138" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/LucyMcGowan/tipr/workflows/R-CMD-check/badge.svg)](https://github.com/LucyMcGowan/tipr/actions)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.04495/status.svg)](https://doi.org/10.21105/joss.04495)
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

This package comes with a few example data sets. For this example, we
will use `exdata_rr`. This data set was simulated such that there are
two confounders, one that was “measured” (and thus usable in the main
analysis, this is called `measured_confounder`) and one that is
“unmeasured” (we have access to it because this is simulated data, but
ordinarily we would not, this variable is called
`.unmeasured_confounder`).

Using the example data `exdata_rr`, we can estimate the exposure-outcome
relationship using the measured confounder as follows:

``` r
mod <- glm(outcome ~ exposure + measured_confounder, data = exdata_rr, 
           family = poisson)

mod %>%
  broom::tidy(exponentiate = TRUE, conf.int = TRUE)
```

    ## # A tibble: 3 × 7
    ##   term                estimate std.error statistic   p.value conf.low conf.high
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
    ## 1 (Intercept)           0.0366    0.151     -21.9  2.56e-106   0.0269    0.0486
    ## 2 exposure              1.49      0.166       2.43 1.52e-  2   1.09      2.08  
    ## 3 measured_confounder   2.43      0.0754     11.7  7.51e- 32   2.09      2.81

We see the above example, the exposure-outcome relationship is 1.5 (95%
CI: 1.1, 2.1). Note, in practice when estimating the effect of an
exposure on a binary outcome using a GLM with the Poisson distribution
and log link function, it is important to use a sandwich estimator to
appropriately estimate the variability (this can be done in R using the
`sandwich` package), which in this case gives a very similar result (95%
CI: 1.1, 2.0).

## Continuous unmeasured confounder example

We are interested in a continuous unmeasured confounder, so we will use
the `tip_with_continuous()` function.

Let’s assume the unmeasured confounder is normally distributed with a
mean of 0.5 in the exposed group and 0 in the unexposed (and unit
variance in both), resulting in a mean difference of 0.5
(`exposure_confounder_effect = 0.5`), let’s solve for the relationship
between the unmeasured confounder and outcome needed to tip the analysis
(in this case, we are solving for `confounder_outcome_effect`).

``` r
tip(effect_observed = 1.5, exposure_confounder_effect = 0.5)
```

    ## ℹ The observed effect (1.5) WOULD be tipped by 1 unmeasured confounder with the
    ##   following specifications:
    ## • estimated difference in scaled means between the unmeasured confounder in the
    ##   exposed population and unexposed population: 0.5
    ## • estimated relationship between the unmeasured confounder and the outcome:
    ##   2.25

    ## # A tibble: 1 × 5
    ##   effect_adjusted effect_observed exposure_confounder_effect confounde…¹ n_unm…²
    ##             <dbl>           <dbl>                      <dbl>       <dbl>   <dbl>
    ## 1               1             1.5                        0.5        2.25       1
    ## # … with abbreviated variable names ¹​confounder_outcome_effect,
    ## #   ²​n_unmeasured_confounders

A hypothetical unobserved continuous confounder a scaled mean difference
between exposure groups of `0.5` would need a relationship of at least
2.25 with the outcome to tip this analysis at the point estimate.

``` r
tip(effect_observed = 1.09, exposure_confounder_effect = 0.5)
```

    ## ℹ The observed effect (1.09) WOULD be tipped by 1 unmeasured confounder with
    ##   the following specifications:
    ## • estimated difference in scaled means between the unmeasured confounder in the
    ##   exposed population and unexposed population: 0.5
    ## • estimated relationship between the unmeasured confounder and the outcome:
    ##   1.19

    ## # A tibble: 1 × 5
    ##   effect_adjusted effect_observed exposure_confounder_effect confounde…¹ n_unm…²
    ##             <dbl>           <dbl>                      <dbl>       <dbl>   <dbl>
    ## 1               1            1.09                        0.5        1.19       1
    ## # … with abbreviated variable names ¹​confounder_outcome_effect,
    ## #   ²​n_unmeasured_confounders

A hypothetical unobserved continuous confounder a scaled mean difference
between exposure groups of `0.5` would need a relationship of at least
1.19 with the outcome to tip this analysis at the 5% level, rendering it
inconclusive.

Because this is simulated data, we can see what the *true* unmeasured
confounder looked like. First we will calculate the difference in scaled
means.

``` r
exdata_rr %>%
  dplyr::group_by(exposure) %>%
  dplyr::summarise(m = mean(.unmeasured_confounder / sd(.unmeasured_confounder))) %>%
  tidyr::pivot_wider(names_from = exposure,
              values_from = m,
              names_prefix = "u_") %>%
  dplyr::summarise(estimate = u_1 - u_0)
```

    ## # A tibble: 1 × 1
    ##   estimate
    ##      <dbl>
    ## 1    0.494

Now we can refit the above model with this unmeasured confounder
included. According to our tipping point result, as long as the risk
ratio of the unmeasured confounder and outcome in the model is greater
than 2.25, the result that we observed will be “tipped” (the point
estimate will cross the null).

``` r
mod_true <- glm(
  outcome ~ exposure + measured_confounder + .unmeasured_confounder, 
  data = exdata_rr, 
  family = poisson)

mod_true %>%
  broom::tidy(exponentiate = TRUE, conf.int = TRUE)
```

    ## # A tibble: 4 × 7
    ##   term                   estimate std.error statistic   p.value conf.low conf.…¹
    ##   <chr>                     <dbl>     <dbl>     <dbl>     <dbl>    <dbl>   <dbl>
    ## 1 (Intercept)              0.0245    0.163    -22.7   1.49e-114   0.0176  0.0334
    ## 2 exposure                 0.921     0.172     -0.477 6.34e-  1   0.660   1.30  
    ## 3 measured_confounder      2.44      0.0746    11.9   6.95e- 33   2.11    2.82  
    ## 4 .unmeasured_confounder   2.42      0.0742    11.9   1.35e- 32   2.09    2.80  
    ## # … with abbreviated variable name ¹​conf.high

Notice here the `.unmeasured_confounder` effect is 2.42 (which is
greater than the 2.25 we calculated that would be needed to render our
result null) and, as expected, the point estimate for the `exposure` has
crossed the null (and now is less than 1).

## Binary unmeasured confounder example

Now we are interested in the binary unmeasured confounder, so we will
use the `tip_with_binary()` function.

Let’s assume the unmeasured confounder is prevalent in 25% of the
exposed population (`exposed_confounder_prev = 0.25`) and in 10% of the
unexposed population (`unexposed_confounder_prev = 0.10`) – let’s solve
for the relationship between the unmeasured confounder and the outcome
needed to tip the analysis (`confounder_outcome_effect`).

``` r
tip_with_binary(effect_observed = 1.09, 
                exposed_confounder_prev = 0.25, 
                unexposed_confounder_prev = 0.10)
```

    ## ℹ The observed effect (1.09) WOULD be tipped by 1 unmeasured confounder with
    ##   the following specifications:
    ## • estimated prevalence of the unmeasured confounder in the exposed population:
    ##   0.25
    ## • estimated prevalence of the unmeasured confounder in the unexposed
    ##   population: 0.1
    ## • estimated relationship between the unmeasured confounder and the outcome:
    ##   1.64

    ## # A tibble: 1 × 6
    ##   effect_adjusted effect_observed exposed_confounder_p…¹ unexp…² confo…³ n_unm…⁴
    ##             <dbl>           <dbl>                  <dbl>   <dbl>   <dbl>   <dbl>
    ## 1               1            1.09                   0.25     0.1    1.64       1
    ## # … with abbreviated variable names ¹​exposed_confounder_prev,
    ## #   ²​unexposed_confounder_prev, ³​confounder_outcome_effect,
    ## #   ⁴​n_unmeasured_confounders

A hypothetical unobserved binary confounder that is prevalent in 10% of
the unexposed population and 25% of the exposed population would need to
have a relationship with the outcome of 1.64 to tip this analysis at the
5% level, rendering it inconclusive.

## Many unmeasured confounders

Suppose we are concerned that there are many small, independent,
continuous, unmeasured confounders present.

``` r
tip(effect_observed = 1.09, 
    exposure_confounder_effect = 0.25, 
    confounder_outcome_effect = 1.05)
```

    ## ℹ The observed effect (1.09) WOULD be tipped by 7 unmeasured confounders with
    ##   the following specifications:
    ## • estimated difference in scaled means between the unmeasured confounder in the
    ##   exposed population and unexposed population: 0.25
    ## • estimated relationship between the unmeasured confounder and the outcome:
    ##   1.05

    ## # A tibble: 1 × 5
    ##   effect_adjusted effect_observed exposure_confounder_effect confounde…¹ n_unm…²
    ##             <dbl>           <dbl>                      <dbl>       <dbl>   <dbl>
    ## 1               1            1.09                       0.25        1.05    7.07
    ## # … with abbreviated variable names ¹​confounder_outcome_effect,
    ## #   ²​n_unmeasured_confounders

It would take about `7` independent standardized Normal unmeasured
confounders with a mean difference between exposure groups of 0.25 and a
relationship with the outcome of 1.05 tip the observed analysis at the
5% level, rendering it inconclusive.

## Integration with broom

These functions were created to easily integrate with models tidied
using the **broom** package. This is not *necessary* to use these
functions, but a nice feature if you choose to do so. Here is an example
of a logistic regression fit with `glm` and tidied with the `tidy`
function **broom** that can be directly fed into the `tip()` function.

``` r
if (requireNamespace("broom", quietly = TRUE) &&  requireNamespace("dplyr", quietly = TRUE)) {
  glm(outcome ~ exposure + measured_confounder, data = exdata_rr, 
           family = poisson) %>%
    broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
    dplyr::filter(term == "exposure") %>%
    dplyr::pull(conf.low) %>%
    tip(confounder_outcome_effect = 2.5)
}
```

    ## ℹ The observed effect (1.09) WOULD be tipped by 1 unmeasured confounder with
    ##   the following specifications:
    ## • estimated difference in scaled means between the unmeasured confounder in the
    ##   exposed population and unexposed population: 0.09
    ## • estimated relationship between the unmeasured confounder and the outcome: 2.5

    ## # A tibble: 1 × 5
    ##   effect_adjusted effect_observed exposure_confounder_effect confounde…¹ n_unm…²
    ##             <dbl>           <dbl>                      <dbl>       <dbl>   <dbl>
    ## 1               1            1.09                     0.0907         2.5       1
    ## # … with abbreviated variable names ¹​confounder_outcome_effect,
    ## #   ²​n_unmeasured_confounders

## Code of Conduct

Please note that the tipr project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
