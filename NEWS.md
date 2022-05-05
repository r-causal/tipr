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
