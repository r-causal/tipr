# Calculate an E-value

Calculate an E-value

## Usage

``` r
e_value(effect_observed)
```

## Arguments

- effect_observed:

  Numeric positive value. Observed exposure - outcome effect (assumed to
  be the exponentiated coefficient, so a risk ratio, odds ratio, or
  hazard ratio). This can be the point estimate, lower confidence bound,
  or upper confidence bound.

## Value

Numeric value

## Examples

``` r
e_value(0.9)
#> [1] 1.462475
e_value(1.3)
#> [1] 1.9245
```
