#' Example Data (Risk Ratio)
#'
#' A data set simulated with two Normally distributed confounders, one
#' "measured" and one "unmeasured", an exposure, and outcome. The "true" causal
#' effect of the exposure on the outcome, accounting for both the measured
#' and unmeasured confounders, should be 0.
#'
#' @format A data frame with 2,000 rows and 4 columns:
#'  * `.unmeasured_confounder`: A simulated unmeasured confounder
#'  * `measured_confounder`: A simulated measured confounder
#'  * `exposure`
#'  * `outcome`
#'
"exdata_rr"
