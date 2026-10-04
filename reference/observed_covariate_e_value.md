# Calculate the Observed Covariate E-value

Calculate the Observed Covariate E-value

## Usage

``` r
observed_covariate_e_value(lb, ub, lb_adj, ub_adj, transform = NULL)
```

## Arguments

- lb:

  Numeric. The lower bound of the full model

- ub:

  Numeric. The upper bound of the full model

- lb_adj:

  Numeric. The lower bound of the adjusted model

- ub_adj:

  Numeric. The upper bound of the adjusted model

- transform:

  Character. If your effect is an odds ratio or hazard ratio, this will
  perform the transformation suggested by VanderWeele and Ding. Allowed
  values are:

  - "OR"

  - "HR"

## Value

The Observed Covariate E-value
