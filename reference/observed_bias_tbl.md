# Create a data frame to assist with creating an observed bias plot

Create a data frame to assist with creating an observed bias plot

## Usage

``` r
observed_bias_tbl(ps_mod, outcome_mod, drop_list = NULL)
```

## Arguments

- ps_mod:

  Model object for the propensity score model

- outcome_mod:

  Model object for the outcome model

- drop_list:

  Named list of covariates or groups of covariates to drop if `NULL`,
  will default to dropping each covariate one at a time.

## Value

Data frame with the following columns:

- `dropped`: The covariate or group of covariates that were dropped

- `type`: Explanation of `dropped`, whether it refers to a single
  covariate (`covariate`) or a group of covariates (`group`)

- `ps_formula`: The new formula for the updated propensity score model

- `outcome_formula`: The new formula for the updated outcome model

- `ps_model`: The new model object for the updated propensity score
  model

- `p`: The updated propensity score

## Examples

``` r
ps_mod <- glm(am ~ mpg + cyl + I(hp^2), data = mtcars)
outcome_mod <- lm(qsec ~ am + hp + disp + wt, data = mtcars)
observed_bias_tbl(
 ps_mod,
 outcome_mod,
 drop_list = list(
   group_one = c("mpg", "hp"),
   group_two = c("cyl", "wt")
   )
 )
#> # A tibble: 2 × 6
#>   dropped   type  ps_formula outcome_formula ps_model p         
#>   <chr>     <chr> <list>     <list>          <list>   <list>    
#> 1 group_one group <formula>  <formula>       <glm>    <dbl [32]>
#> 2 group_two group <formula>  <formula>       <glm>    <dbl [32]>
```
