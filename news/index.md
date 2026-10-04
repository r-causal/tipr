# Changelog

## tipr (development version)

## tipr 1.0.2

CRAN release: 2024-02-06

- [`adjust_coef_with_binary()`](../reference/adjust_coef_with_binary.md)
  now assumes the coefficient is from a linear model rather than
  loglinear. Use `loglinear = TRUE` to get the old behavior.
  ([\#12](https://github.com/r-causal/tipr/issues/12),
  [@malcolmbarrett](https://github.com/malcolmbarrett))
- Fixed roxygen issue with package documentation
- Update messaging and errors

## tipr 1.0.1

CRAN release: 2022-09-05

- Fixed bug, functions based on the `adjust_coef_with_binary` function
  had the old parameter names (`exposed_p` and `unexposed_p`). These
  were changed to match the other new updates from version 1.0.0 to now
  be `exposed_confounder_prev` and `unexposed_confounder_prev`.
- Change “relative risk” to “risk ratio” in all documentation.
- Add new JOSS citation

## tipr 1.0.0

CRAN release: 2022-08-06

**Breaking changes**. The names of several arguments were changed for
increased clarity:

- `effect` -\> `effect_observed`

- `outcome_association` -\> `confounder_outcome_effect`

- `smd` -\> `exposure_confounder_effect`

- `exposed_p` -\> `exposed_confounder_prev`

- `unexposed_p` -\> `unexposed_confounder_prev`

- `exposure_r2` -\> `confounder_exposure_r2`

- `outcome_r2` -\> `confounder_outcome_r2`

- Added two new example datasets: `exdata_continuous` and `exdata_rr`

## tipr 0.4.2

- Make the output tibble names consistent (`adjusted_effect` -\>
  `effect_adjusted`)

## tipr 0.4.1

CRAN release: 2022-05-05

- Add additional functions that specify `*_with_continuous()` (long form
  of, the function names, the default unmeasured confounder is Normally
  distributed)
- Change `tip_lm()` to [`tip_coef()`](../reference/tip_coef.md).

## tipr 0.4.0

CRAN release: 2022-04-16

- Changed the name of `lm_tip()` to `tip_lm()`
- The API has been fundamentally updated so that the functions now take
  a numeric value as a first argument rather than a data frame.
- Added adjust\_\* functions to allow for specification of all
  unmeasured confounder qualities without tipping
- Split `tip_*` functions into hazard ratio, odds ratio, and relative
  risk
- Add R2 parameterization with
  [`tip_coef_with_r2()`](../reference/tip_coef_with_r2.md),
  [`adjust_coef_with_r2()`](../reference/adjust_coef_with_r2.md), and
  [`r_value()`](../reference/r_value.md)

## tipr 0.3.0

CRAN release: 2021-09-10

- Added ability to perform sensitivity analyses on linear models via
  `lm_tip()`

## tipr 0.2.0

CRAN release: 2020-11-16

- Updated several function and parameter names. The main functions are
  now [`tip()`](../reference/tip.md) and
  [`tip_with_binary()`](../reference/tip_with_binary.md). The parameter
  names are more self-explanatory.
- The API has been fundamentally updated so that the functions now take
  a data frame as a first argument.
- There is now explicit (but not required) integration with the `broom`
  package.

## tipr 0.1.1

CRAN release: 2017-11-28

- initial CRAN release
