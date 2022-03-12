#' Robustness value
#'
#' This function wraps the [`sensemakr::robustness_value()`] function
#'
#' @param effect Numeric. Observed exposure - outcome effect from a regression
#'    model. This is the point estimate (beta coefficient)
#' @param se Numeric. Standard error of the `effect` in the previous parameter.
#' @param df Numeric positive value. Residual degrees of freedom for the model
#'    used to estimate the observed exposure - outcome effect. This is the total
#'    number of observations minus the number of parameters estimated in your
#'    model. Often for models estimated with an intercept this is N - k - 1
#'    where k is the number of predictors in the model.
#' @param ... Optional arguments passed to the [`sensemakr::robustness_value()`]
#'    function.
#' @references Carlos Cinelli, Jeremy Ferwerda and Chad Hazlett (2021).
#'    sensemakr: Sensitivity Analysis
#'    Tools for Regression Models. R package version 0.1.4.
#'    https://CRAN.R-project.org/package=sensemakr
#'
#' @return Numeric. Robustness value
#' @export
#'
#' @examples
#' r_value(0.5, 0.1, 102)

r_value <- function(effect, se, df, ...) {
  as.numeric(sensemakr::robustness_value(effect / se, dof = df, ...))
}
