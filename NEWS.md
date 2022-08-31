# tipr 1.0.1

* Fixed bug, functions based on the `adjust_coef_with_binary` function had the old parameter names (`exposed_p` and `unexposed_p`). These were changed to match the other new updates from version 1.0.0 to now be `exposed_confounder_prev` and `unexposed_confounder_prev`.

# tipr 1.0.0

**Breaking changes**. The names of several arguments were changed for increased clarity:

* `effect` -> `effect_observed`
* `outcome_association` -> `confounder_outcome_effect`
* `smd` -> `exposure_confounder_effect`
* `exposed_p` -> `exposed_confounder_prev`
* `unexposed_p` -> `unexposed_confounder_prev`
* `exposure_r2` -> `confounder_exposure_r2`
* `outcome_r2` -> `confounder_outcome_r2`

* Added two new example datasets: `exdata_continuous` and `exdata_rr`

# tipr 0.4.2

* Make the output tibble names consistent (`adjusted_effect` -> `effect_adjusted`)

# tipr 0.4.1

* Add additional functions that specify `*_with_continuous()` (long form of, the function names, the default unmeasured confounder is Normally distributed)
* Change `tip_lm()` to `tip_coef()`.

# tipr 0.4.0

* Changed the name of `lm_tip()` to `tip_lm()`
* The API has been fundamentally updated so that the functions now take a numeric value as a first argument rather than a data frame.
* Added adjust_* functions to allow for specification of all unmeasured confounder qualities without tipping
* Split `tip_*` functions into hazard ratio, odds ratio, and relative risk
* Add R2 parameterization with `tip_coef_with_r2()`, `adjust_coef_with_r2()`, and `r_value()`

# tipr 0.3.0

* Added ability to perform sensitivity analyses on linear models via `lm_tip()`

# tipr 0.2.0

* Updated several function and parameter names. The main functions are now `tip()` and `tip_with_binary()`. The parameter names are more self-explanatory.
* The API has been fundamentally updated so that the functions now take a data frame as a first argument.
* There is now explicit (but not required) integration with the `broom` package.

# tipr 0.1.1

* initial CRAN release
