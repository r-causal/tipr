#' Adjust a regression coefficient using the partial R2
#' for an unmeasured confounder-exposure relationship and unmeasured confounder-
#' outcome relationship
#'
#' This function wraps the [`sensemakr::adjusted_estimate()`] and
#'    [`sensemakr::adjusted_se()`] functions.
#'
#' @param effect_observed Numeric. Observed exposure - outcome effect from a regression
#'    model. This is the point estimate (beta coefficient)
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
#' @param ... Optional arguments passed to the [`sensemakr::adjusted_estimate()`]
#'    function.
#' @references Carlos Cinelli, Jeremy Ferwerda and Chad Hazlett (2021).
#'    sensemakr: Sensitivity Analysis
#'    Tools for Regression Models. R package version 0.1.4.
#'    https://CRAN.R-project.org/package=sensemakr
#' @return A data frame.
#' @export
#'
#' @examples
#' adjust_coef_with_r2(0.5, 0.1, 102, 0.05, 0.1)
adjust_coef_with_r2 <- function(effect_observed,
                                se,
                                df,
                                confounder_exposure_r2,
                                confounder_outcome_r2,
                                verbose = TRUE,
                                alpha = 0.05,
                                ...) {
  effect_adjusted <- sensemakr::adjusted_estimate(
    estimate = effect_observed,
    se = se,
    dof = df,
    r2dz.x = confounder_exposure_r2,
    r2yz.dx = confounder_outcome_r2,
    ...
  )
  se_adjusted <- sensemakr::adjusted_se(
    estimate = effect_observed,
    se = se,
    dof = df,
    r2dz.x = confounder_exposure_r2,
    r2yz.dx = confounder_outcome_r2,
    ...
  )
  t_star <- stats::qt(alpha / 2, df = df, lower.tail = F)
  lb_observed <- effect_observed - t_star * se
  ub_observed <- effect_observed + t_star * se
  lb_adjusted <- effect_adjusted - t_star * se_adjusted
  ub_adjusted <- effect_adjusted + t_star * se_adjusted
  #TODO: verbose

  tibble::tibble(
    effect_adjusted = effect_adjusted,
    lb_adjusted = lb_adjusted,
    ub_adjusted = ub_adjusted,
    effect_observed = effect_observed,
    lb_observed = lb_observed,
    ub_observed = ub_observed,
    se_observed = se,
    df_observed = df,
    confounder_exposure_r2 = confounder_exposure_r2,
    confounder_outcome_r2 = confounder_outcome_r2
  )
}
