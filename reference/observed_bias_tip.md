# Create a data frame to combine with an observed bias data frame demonstrating a hypothetical unmeasured confounder

Create a data frame to combine with an observed bias data frame
demonstrating a hypothetical unmeasured confounder

## Usage

``` r
observed_bias_tip(
  tip,
  point_estimate,
  lb,
  ub,
  tip_desc = "Hypothetical unmeasured confounder"
)
```

## Arguments

- tip:

  Numeric. Value you would like to tip to.

- point_estimate:

  Numeric. Result estimate from the full model.

- lb:

  Numeric. Result lower bound from the full model.

- ub:

  Numeric. Result upper bound from the full model.

- tip_desc:

  Character. A description of the tipping point.

## Value

A data frame with five columns:

- `dropped`: the input from `tip_desc`

- `type`: Explanation of `dropped`, here `tip` to clarify that this was
  calculated as a tipping point.

- `point_estimate`: the shifted point estimate

- `lb`: the shifted lower bound

- `ub`: the shifted upper bound
