#' Tip a regression coefficient using the partial R2
#' for an unmeasured confounder-exposure relationship and unmeasured confounder-
#' outcome relationship
#'
#' Choose one of the following, and the other will be estimated:
#' * `exposure_r2`
#' * `outcome_r2`
#'
#' @param effect Numeric. Observed exposure - outcome effect from a regression
#'    model. This is the point estimate (beta coefficient)
#' @param se Numeric. Standard error of the `effect` in the previous parameter.
#' @param df Numeric positive value. Residual degrees of freedom for the model
#'    used to estimate the observed exposure - outcome effect. This is the total
#'    number of observations minus the number of parameters estimated in your
#'    model. Often for models estimated with an intercept this is N - k - 1
#'    where k is the number of predictors in the model.
#' @param exposure_r2 Numeric value between 0 and 1. The assumed partial R2 of
#'    the unobserved confounder with the exposure given the measured covariates.
#' @param outcome_r2 Numeric value between 0 and 1. The assumed partial R2 of
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
tip_coef_with_r2 <- function(effect,
                             se,
                             df,
                             exposure_r2 = NULL,
                             outcome_r2 = NULL,
                             verbose = TRUE,
                             alpha = 0.05,
                             tip_bound = FALSE,
                             ...) {

  if (is.null(exposure_r2)) {
    if (tip_bound) {
      exposure_r2 <- tip_exposure_r2_bound(effect, se, df, outcome_r2, alpha)
    } else {
      exposure_r2 <- tip_exposure_r2(effect, se, df, outcome_r2)
    }
  } else if (is.null(outcome_r2)) {
    if (tip_bound) {
      outcome_r2 <- tip_outcome_r2_bound(effect, se, df, exposure_r2, alpha)
    } else{
      outcome_r2 <-  tip_outcome_r2(effect, se, df, exposure_r2)
    }
  }
  o <- adjust_coef_with_r2(
    effect = effect,
    se = se,
    df = df,
    exposure_r2 = exposure_r2,
    outcome_r2 = outcome_r2,
    verbose = verbose,
    alpha = alpha,
    ...
  )
  if (tip_bound && (round(o$lb_adjusted, 8) != 0 && round(o$ub_adjusted, 8) != 0)) {
    print(o)
    o <- adjust_coef_with_r2(
      effect = effect,
      se = se,
      df = df,
      exposure_r2 = exposure_r2,
      outcome_r2 = 0,
      verbose = verbose,
      alpha = alpha,
      ...
    )
  }
  return(o)
}


