---
title: 'tipr: An R package for sensitivity analyses for unmeasured confounders'
tags:
  - R
  - statistics
  - epidemiology
  - sensitivity analyses
  - causal inference
  - confounding
authors:
  - name: Lucy D\'Agostino McGowan^[Corresponding author]
    orcid: 0000-0001-7297-9359
    affiliation: 1
affiliations:
 - name: Wake Forest University, USA
   index: 1
date: 04 May 2022
bibliography: paper.bib

---

# Summary

The strength of evidence provided by epidemiological and observational studies is inherently limited by the potential for unmeasured confounding. We focus on three key quantities: the observed bound of the confidence interval closest to the null, the relationship between an unmeasured confounder and the outcome, for example a plausible residual effect size for an unmeasured continuous or binary confounder, and the relationship between an unmeasured confounder and the exposure, for example a realistic mean difference or prevalence difference for this hypothetical confounder between exposure groups. Building on the methods put forth by @Cornfield, @Bross, @Schlesselman, @Rosenbaum:1983, @Lin, @lash2009applying, @rosenbaum1986dropping, @cinelli2020making, @VanderWeele:2017ki, and @Ding, we can use these quantities to assess how an unmeasured confounder may tip our result to insignificance, rendering the study inconclusive.

# Statement of need

When assessing the relationship between an exposure and an outcome, the "no unmeasured confounders" assumption is crucial.[@rubin1974estimating; @DAgostino:1998tu] This assumption, however, is untestable, rendering sensitivity analyses necessary to quantify the potential impact of an unmeasured confounder. There are several related methods for conducting sensitivity analyses for unmeasured confounders[@Cornfield; @Bross; @Schlesselman; @Rosenbaum:1983; @Lin; @lash2009applying; @rosenbaum1986dropping; @cinelli2020making; @VanderWeele:2017ki; @Ding], however there is not currently a single R package that has a unified grammar allowing the user to conduct appropriate sensitivity analysis for their study. A unified grammar allows the user to easily transition between methods depending on the scenario under which they are conducting the sensitivity analysis as well as compare the results between methods. `tipr` seeks to fill this need.

# Overview 

The `tipr` R package [@rstats] allows the user to conduct sensitivity analyses for unmeasured confounders. The user provides an "observed effect" (that is, some effect between an exposure of interest and an outcome of interest after adjusting for any observed confounders), and the sensitivity analyses will quantify how sensitive the effect is to a potential unmeasured confounder.  The `tipr` functions fall into three categories:

(1) Functions that calculate how an observed effects would change with a specified unmeasured confounder (`adjust` functions)
(2) Functions that calculate the magnitude of an unmeasured confounder needed to tip an observed effect to cross the null, rendering it inconclusive (`tip` functions)
(3) Functions that calculate single number summaries of an observed effect's "sensitivity", such as the E-value[@VanderWeele:2017ki] or Robustness value [@cinelli2020making].

`tipr` is available on [CRAN](https://github.com/lucymcgowan/tipr) and [Github](https://github.com/lucymcgowan/tipr). Documentation can be found at 
https://lucymcgowan.github.io/tipr/. It can be installed from CRAN using the 
following code:

```{r}
install.packages("tipr")
```

The development version can be installed from Github using the following code:

```{r}
devtools::install_github("LucyMcGowan/tipr")
```

The library can be loaded by running:

```{r}
library(tipr)
```

# Unmeasured confounder parameterization

Informally, a confounder is a pre-exposure variable that is associated with both the exposure of interest and outcome of interest but is *not* on the causal pathway between the two.[@vanderweele2013definition; @van2010confounding]. There are two quantities that measure the strength of an unmeasured confounder -- the relationship between the unmeasured confounder and the exposure and the strength of the unmeasured confounder and the outcome. The `tipr` package allows these quantities to be specified in a number of ways. 

## Unmeasured confounder - exposure relationship

If quantifying the impact of a Normally distributed confounder, the impact of the unmeasured confounder on the exposure is parameterized as a difference in standardized means between the unmeasured confounder in the exposed population and the unexposed population. This is specified using the `smd` parameter. If quantifying the impact of a binary confounder, the impact is quantified using two parameters, `exposed_p`: The estimated prevalence of the unmeasured confounder in the unexposed population and `unexposed_p`: The estimated prevalence of the unmeasured confounder in the unexposed population. Finally, the user can choose to quantify the magnitude of the unmeasured confounder-exposure relationship in terms of the percent of variation in the exposure explained by the unmeasured confounder. This is specified using the `exposure_r2` parameter. 

## Unmeasured confounder - outcome relationship

If quantifying the impact of a Normally distributed confounder or binary confounder, the impact of the unmeasured confounder on the outcome is parameterized as the anticipated effect size of the unmeasured confounder if it were included in the final outcome model. This effect will be on the same scale as the input observed effect. For example, if the observed effect is a *coefficient* in a linear regression model, this association would also be a coefficient; if the observed effect is a *hazard ratio* from a Cox proportional hazards model, this association would also be a hazard ratio. This is specified using the `outcome_association` parameter. Alternatively, the user can choose to quantify the magnitude of the unmeasured confounder-outcome relationship in terms of the percent of variation in the outcome explained by the unmeasured confounder. This is specified using the `outcome_r2` parameter. 

# Syntax

The functions in the `tipr` package follow a unified grammar. The function names follow this form: `{action}_{effect}_with_{what}`. For example, to adjust (`action`) a coefficient (`effect`) with a binary unmeasured confounder (`what`), we use the function `adjust_coef_with_binary()`. The "default" function assumes the confounder type is Normally distributed, therefore `adjust_coef()` is equivalent to `adjust_coef_with_continuous()`.

The first argument of each function is `effect` quantifying the observed exposure-outcome relationship. The functions intended to adjust an effect with partial $R^2$ values specified are built on the `sensemakr` package[@sensemakr] and have the following additional arguments: `se`: The standard error fo the observed exposure - outcome relationship and `df`: The residual degrees of freedom from the model used to fit the observed exposure - outcome relationship. This is the total number of observations minus the number of parameters estimated in your model. Often for models estimated with an intercept this is $N - k - 1$ where $k$ is the number of predictors in the model. The subsequent parameters in each function describe the unmeasured confounder's relationship with the exposure and outcome for the `adjust` functions, and one or the other for the `tip` functions.

The output for all functions is a data frame with the adjusted effect in the first column, the observed effect in the second column, and the specified sensitivity parameters in the subsequent columns. For example, if we want to know the impact of an unmeasured confounder with `smd = 0.1` and `outcome_association = 1` on an observed `effect = 1.5`, we would run the following code.

```r
library(tipr)
adjust_coef(effect = 1.5,
            smd = 0.1,
            outcome_association = 1)
```

    ## The observed effect (1.5) is updated to 1.4 by a confounder with the 
    ## following specifications:
    ##  * estimated difference in scaled means: 0.1
    ##  * estimated association between the unmeasured confounder and the 
    ##    outcome: 1
    ## # A tibble: 1 × 4
    ## effect_adjusted effect_observed   smd outcome_association
    ##             <dbl>           <dbl> <dbl>               <dbl>
    ## 1             1.4             1.5   0.1                   1


# Functions

An overview of the main functions in `tipr` is summarized in **Table 1**:

+-------------------------------+----------------------------------------------+
| Function name                 | Use                                          |
+===============================+==============================================+
| **`adjust_coef`**,            | These functions adjust an observed           |
|                               | coefficient from a linear, log-linear,       |
|                               | logistic, or Cox proportional hazards model  |
| `adjust_coef_with_continuous`,| using a specified unmeasured confounder.     |
|                               |                                              |
| `adjust_coef_with_binary`,    |                                              |
|                               |                                              |
| `adjust_coef_with_r2`         |                                              |
+-------------------------------+----------------------------------------------+
| **`adjust_rr`**               | These functions adjust an observed           |
|                               | relative risk using a specified unmeasured   |      
| `adjust_rr_with_continuous`,  | confounder.                                  |
|                               |                                              |
| `adjust_rr_with_binary`       |                                              |
+-------------------------------+----------------------------------------------+
| **`adjust_or`**               | These functions adjust an observed           |
|                               | odds ratio using a specified unmeasured      |      
| `adjust_or_with_continuous`,  | confounder.                                  |
|                               |                                              |
| `adjust_or_with_binary`       |                                              |
+-------------------------------+----------------------------------------------+
| **`adjust_hr`**               | These functions adjust an observed           |
|                               | hazard ratio using a specified unmeasured    |      
| `adjust_hr_with_continuous`,  | confounder.                                  |
|                               |                                              |
| `adjust_hr_with_binary`       |                                              |
+-------------------------------+----------------------------------------------+
| **`tip_coef`**,               | These functions calculate the unmeasured     |
|                               | confounder that would tip an observed        |
|  `tip_coef_with_continuous`,  | coefficient from a linear, log-linear,       |
|                               | logistic, or Cox proportional hazards model. |
|  `tip_coef_with_r2`           | Either the unmeasured confounder - exposure  |
|                               | or unmeasured confounder - outcome need to   |
|                               | be specified and the other relationship      |
|                               | needed make the coefficient (or bound) cross |
|                               | 0 will be output.                            |
+-------------------------------+----------------------------------------------+
| **`tip_rr`**,                 | These functions calculate the unmeasured     |
|                               | confounder that would tip an observed        |  
| `tip_rr_with_continuous`,     | relative risk. Either the unmeasured         |
|                               | confounder - exposure or unmeasured          |  
| `tip_rr_with_binary`          | confounder - outcome relationship need to be |  
|                               | specified and the other relationship         |
|                               | needed make the coefficient (or bound) cross |
|                               | 0 will be output.                            |
+-------------------------------+----------------------------------------------+
| **`tip_or`**,                 | These functions calculate the unmeasured     |
|                               | confounder that would tip an observed        |  
| `tip_or_with_continuous`,     | odds ratio. Either the unmeasured            |
|                               | confounder - exposure or unmeasured          |  
| `tip_or_with_binary`          | confounder - outcome relationship need to be |  
|                               | specified and the other relationship         |
|                               | needed make the coefficient (or bound) cross |
|                               | 0 will be output.                            |
+-------------------------------+----------------------------------------------+
| **`tip_hr`**,                 | These functions calculate the unmeasured     |
|                               | confounder that would tip an observed        |  
| `tip_hr_with_continuous`,     | hazard ratio. Either the unmeasured          |
|                               | confounder - exposure or unmeasured          |  
| `tip_hr_with_binary`          | confounder - outcome relationship need to be |  
|                               | specified and the other relationship         |
|                               | needed make the coefficient (or bound) cross |
|                               | 0 will be output.                            |
+-------------------------------+----------------------------------------------+
| **`e_value`**                 | These functions calculate single number      |
|                               | summaries of the sensitivity of a particular |
| **`r_value`**                 | observed effect to unmeasured confounding,   |
|                               | the E-value and Robustness value,            |
|                               |respectively                                  |
+-------------------------------+----------------------------------------------+

# Tipping Point Example

After fitting your model, you can determine the unmeasured confounder
needed to tip your analysis. In this example, a model was fit and the exposure-outcome relationship was a relative risk of 1.5 (95% CI: 1.2, 1.8).

We are interested in a Normally distributed unmeasured confounder, so we can use the `tip_rr_with_continuous()` function. The function `tip()` is an alias for this function.

Let’s assume the relationship between the unmeasured confounder and outcome is 1.5 (`outcome_association = 1.5`), let's solve for the association between the exposure and unmeasured confounder needed to tip the analysis (in this case, we are solving for `smd`, the mean difference needed between the exposed and unexposed).

``` r
tip(1.2, outcome_association = 1.5)
```

    ## The observed effect (1.2) WOULD be tipped by 1 unmeasured confounder
    ## with the following specifications:
    ##   * estimated difference in scaled means between the unmeasured 
    ##     confounder in the exposed population and unexposed population: 0.45
    ##   * estimated association between the unmeasured confounder and 
    ##     the outcome: 1.5
    ## 
    ## 
    ## # A tibble: 1 × 5
    ##   effect_adjusted effect_observed   smd outcome_association 
    ##             <dbl>           <dbl> <dbl>               <dbl>                    
    ## 1               1             1.2 0.450                 1.5
    ## # … with 1 more variable: n_unmeasured_confounders <dbl>

A hypothetical unobserved continuous confounder that has an association
of 1.5 with the outcome would need a scaled mean difference between
exposure groups of `0.45` to tip this analysis at the 5% level,
rendering it inconclusive.

# Conclusion

The `tipr` package facilitates sensitivity analyses for unmeasured confounding, building on the methods put forth by
@Cornfield, @Bross, @Schlesselman, @Rosenbaum:1983, @Lin, @lash2009applying, @rosenbaum1986dropping, @cinelli2020making, @VanderWeele:2017ki, and @Ding. 

# References
