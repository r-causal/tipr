#' Calculate an E-value
#'
#' @param effect Numeric positive value. Observed exposure - outcome effect
#'    (assumed to be the exponentiated coefficient, so a relative risk, odds
#'    ratio, or hazard ratio). This can be the point estimate, lower confidence
#'    bound, or upper confidence bound.
#'
#' @return Numeric value
#' @export
#'
#' @examples
#' e_value(0.9)
#' e_value(1.3)
e_value <- function(effect) {
  if (effect <= 1) {
    effect <- 1 / effect
  }
  effect + sqrt(effect * (effect - 1))
}
