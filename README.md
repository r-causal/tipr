
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tipr: R tools for tipping point sensitivity analyses

[![Build
Status](https://travis-ci.org/LucyMcGowan/tipr.svg?branch=master)](https://travis-ci.org/LucyMcGowan/tipr)

**Authors:** [Lucy D’Agostino McGowan](http://www.lucymcgowan.com)<br/>
**License:** [MIT](https://opensource.org/licenses/MIT)

## Installation

``` r
# install.packages(devtools)
devtools::install_github("lucymcgowan/tipr")
```

``` r
library("tipr")
```

## Usage

After fitting your model, you can determine the unmeasured confounder
needed to tip your analysis. This unmeasured confounder is determined by
two quantities, the association between the exposure and the unmeasured
confounder (if the unmeasured confounder is continuous, this is
indicated with `smd`, if binary, with `exposed_p` and `unexposed_p`),
and the association between the unmeasured confounder and outcome
`outcome_association`. Using this 📦, we can fix one of these and solve
for the other. Alternatively, we can fix both and solve for `n`, that
is, how many unmeasured confounders of this magnitude would tip the
analysis.

In this example, a model was fit and the exposure-outcome relationship
was 1.5 (95% CI: 1.2, 1.8).

## Continuous unmeasured confounder example

We are interested in a continuous unmeasured confounder, so we will use
the `tip_with_continuous()` function.

Let’s assume the relationship between the unmeasured confounder and
outcome is 1.5 (`outcome_association = 1.5`), let’s solve for the
association between the exposure and unmeasured confounder needed to tip
the analysis (in this case, we are solving for `smd`, the mean
difference needed between the exposed and unexposed).

``` r
data.frame(conf.low = 1.2,
           conf.high = 1.8) %>%
tip(outcome_association = 1.5)
```

    ## The observed effect (1.2, 1.8) WOULD be tipped by 1 unmeasured confounder
    ## with the following specifications:
    ##   * estimated standardized mean difference between the unmeasured confounder
    ##     in the exposed population and unexposed population: 0.45
    ##   * estimated association between the unmeasured confounder and the outcome: 1.5

    ## # A tibble: 1 x 5
    ##   observed_lb observed_ub   smd outcome_association n_unmeasured_confounders
    ##         <dbl>       <dbl> <dbl>               <dbl>                    <dbl>
    ## 1         1.2         1.8 0.450                 1.5                        1

A hypothetical unobserved continuous confounder that has an association
of 1.5 with the outcome would need a scaled mean difference between
exposure groups of `0.45` to tip this analysis at the 5% level,
rendering it inconclusive.

## Binary unmeasured confounder example

Now we are interested in the binary unmeasured confounder, so we will
use the `tip_with_binary()` function.

Let’s assume the unmeasured confounder is prevalent in 25% of the
exposed population (`exposed_p = 0.25`) and in 10% of the unexposed
population (`unexposed_p = 0.10`) – let’s solve for the association
between the unmeasured confounder and the outcome needed to tip the
analysis (`outcome_association`).

``` r
data.frame(conf.low = 1.2,
           conf.high = 1.8) %>%
tip_with_binary(exposed_p = 0.25, unexposed_p = 0.10)
```

    ## The observed effect (1.2, 1.8) WOULD be tipped by 1 unmeasured confounder
    ## with the following specifications:
    ##   * estimated prevalence of the unmeasured confounder in the exposed population: 0.25
    ##   * estimated prevalence of the unmeasured confounder in the unexposed population: 0.1
    ##   * estimated association between the unmeasured confounder and the outcome: 2.54

    ## # A tibble: 1 x 6
    ##   observed_lb observed_ub exposed_p unexposed_p outcome_associa…
    ##         <dbl>       <dbl>     <dbl>       <dbl>            <dbl>
    ## 1         1.2         1.8      0.25         0.1             2.54
    ## # … with 1 more variable: n_unmeasured_confounders <dbl>

A hypothetical unobserved binary confounder that is prevalent in 10% of
the unexposed population and 25% of the exposed population would need to
have an association with the outcome of 2.5 to tip this analysis at the
5% level, rendering it inconclusive.

## Many unmeasured confounders

Suppose we are concerned that there are many small, independent,
continuous, unmeasured confounders present.

``` r
data.frame(conf.low = 1.2,
           conf.high = 1.8) %>%
tip(smd = 0.25, outcome_association = 1.05)
```

    ## The observed effect (1.2, 1.8) WOULD be tipped by 15 unmeasured confounders
    ## with the following specifications:
    ##   * estimated standardized mean difference between the unmeasured confounder
    ##     in the exposed population and unexposed population: 0.25
    ##   * estimated association between the unmeasured confounder and the outcome: 1.05

    ## # A tibble: 1 x 5
    ##   observed_lb observed_ub   smd outcome_association n_unmeasured_confounders
    ##         <dbl>       <dbl> <dbl>               <dbl>                    <dbl>
    ## 1         1.2         1.8  0.25                1.05                     14.9

It would take about `15` independent unmeasured confounders with a
scaled mean difference between exposure groups of 0.25 to and an
association with the outcome of 1.05 tip the observed analysis at the 5%
level, rendering it inconclusive.

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
    tip(outcome_association = 2.5)
}
```

    ## The observed effect (1.13, 1.8) WOULD be tipped by 1 unmeasured confounder
    ## with the following specifications:
    ##   * estimated standardized mean difference between the unmeasured confounder
    ##     in the exposed population and unexposed population: 0.13
    ##   * estimated association between the unmeasured confounder and the outcome: 2.5

    ## # A tibble: 1 x 5
    ##   observed_lb observed_ub   smd outcome_association n_unmeasured_confounders
    ##         <dbl>       <dbl> <dbl>               <dbl>                    <dbl>
    ## 1        1.13        1.80 0.133                 2.5                        1
