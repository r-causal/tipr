#' Create a data frame to combine with an observed bias data frame demonstrating a hypothetical unmeasured confounder
#'
#' @param tip Numeric. Value you would like to tip to.
#' @param point_estimate Numeric. Result estimate from the full model.
#' @param lb Numeric. Result lower bound from the full model.
#' @param ub Numeric. Result upper bound from the full model.
#' @param tip_desc Character. A description of the tipping point.
#'
#' @return A data frame with five columns:
#' * `dropped`: the input from `tip_desc`
#' * `type`: Explanation of `dropped`, here `tip` to clarify that this was calculated as a tipping point.
#' * `point_estimate`: the shifted point estimate
#' * `lb`: the shifted lower bound
#' * `ub`: the shifted upper bound
#' @export
#'
observed_bias_tip <- function(tip, point_estimate, lb, ub, tip_desc = "Hypothetical unmeasured confounder") {
  shift <- 1 - tip
  tibble::tibble(
    dropped = tip_desc,
    type = "tip",
    point_estimate = point_estimate + shift,
    lb = lb + shift,
    ub = ub + shift
  )
}
