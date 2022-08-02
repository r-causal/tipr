#' Tip a regression coefficient using the partial R2
#' for an unmeasured confounder-exposure relationship and unmeasured confounder-
#' outcome relationship
#'
#' Choose one of the following, and the other will be estimated:
#' * `confounder_exposure_r2`
#' * `confounder_outcome_r2`
#'
#' @param effect_observed Numeric. Observed exposure - outcome effect from a
#'    regression model. This is the point estimate (beta coefficient)
#' @param se Numeric. Standard error of the `effect_observed` in the previous parameter.
#' @param df Numeric positive value. Residual degrees of freedom for the model
#'    used to estimate the observed exposure - outcome effect. This is the total
#'    number of observations minus the number of parameters estimated in your
#'    model. Often for models estimated with an intercept this is N - k - 1
#'    where k is the number of predictors in the model.
#' @param confounder_exposure_r2 Numeric value between 0 and 1. The assumed partial R2 of
#'    the unobserved confounder with the exposure given the measured covariates.
#' @param confounder_outcome_r2 Numeric value between 0 and 1. The assumed partial R2 of
#'    the unobserved confounder with the outcome given the exposure and
#'    the measured covariates.
#' @param verbose Logical. Indicates whether to print informative message.
#'    Default: `TRUE`
#' @param alpha Significance level. Default = `0.05`.
#' @param tip_bound Do you want to tip at the bound? Default = `FALSE`, will tip at the point estimate
#' @param ... Optional arguments passed to the [`sensemakr::adjusted_estimate()`]
#'    function.
#' @return A data frame.
#' @export
#'
#' @examples
#' tip_coef_with_r2(0.5, 0.1, 102, 0.5)
tip_coef_with_r2 <- function(effect_observed,
                             se,
                             df,
                             confounder_exposure_r2 = NULL,
                             confounder_outcome_r2 = NULL,
                             verbose = TRUE,
                             alpha = 0.05,
                             tip_bound = FALSE,
                             ...) {
  if (is.null(confounder_exposure_r2)) {
    if (tip_bound) {
      confounder_exposure_r2 <-
        tip_exposure_r2_bound(effect_observed, se, df, confounder_outcome_r2, alpha)
    } else {
      confounder_exposure_r2 <-
        tip_exposure_r2(effect_observed, se, df, confounder_outcome_r2)
    }
  } else if (is.null(confounder_outcome_r2)) {
    if (tip_bound) {
      confounder_outcome_r2 <-
        tip_outcome_r2_bound(effect_observed, se, df, confounder_exposure_r2, alpha)
    } else{
      confounder_outcome_r2 <-
        tip_outcome_r2(effect_observed, se, df, confounder_exposure_r2)
    }
  }
  o <- adjust_coef_with_r2(
    effect_observed = effect_observed,
    se = se,
    df = df,
    confounder_exposure_r2 = confounder_exposure_r2,
    confounder_outcome_r2 = confounder_outcome_r2,
    verbose = verbose,
    alpha = alpha,
    ...
  )
  if (tip_bound &&
      (round(o$lb_adjusted, 8) != 0 && round(o$ub_adjusted, 8) != 0)) {
    print(o)
    o <- adjust_coef_with_r2(
      effect_observed = effect_observed,
      se = se,
      df = df,
      confounder_exposure_r2 = confounder_exposure_r2,
      confounder_outcome_r2 = 0,
      verbose = verbose,
      alpha = alpha,
      ...
    )
  }
  return(o)
}
