#' Calculate the Observed Covariate E-value
#'
#' @param lb Numeric. The lower bound of the full model
#' @param ub Numeric. The upper bound of the full model
#' @param lb_adj Numeric. The lower bound of the adjusted model
#' @param ub_adj Numeric. The upper bound of the adjusted model
#' @param transform Character. If your effect is an odds ratio or hazard ratio, this will
#' perform the transformation suggested by VanderWeele and Ding. Allowed values are:
#'  * "OR"
#'  * "HR"
#'
#' @return The Observed Covariate E-value
#' @export
observed_covariate_e_value <- function(lb, ub, lb_adj, ub_adj, transform = NULL) {
  if (!is.null(transform)) {
    if (!transform %in% c("OR", "HR")) {
      stop_cli(c(
        "x" = "You input `transform`: {transform}\n ",
        "i" = "The only valid `transform` inputs are 'HR' and 'OR'."
      ))
    }
    if (transform == "OR") {
      lb <- sqrt(lb)
      ub <- sqrt(ub)
      lb_adj <- sqrt(lb_adj)
      ub_adj <- sqrt(ub_adj)
    }
    if (transform == "HR") {
      lb <- hr_transform(lb)
      ub <- hr_transform(ub)
      lb_adj <- hr_transform(lb_adj)
      ub_adj <- hr_transform(ub_adj)
    }
  }
  b <- get_limiting_bound(lb, ub)
  b_adj <- get_limiting_bound_adj(b, lb_adj, ub_adj)
  if (b < 1) {
    b <- 1 / b
    b_adj <- 1 / b_adj
  }
  if (b < b_adj) {
    return((b_adj / b) + sqrt((b_adj / b) * ((b_adj / b) - 1)))
  }
  (b / b_adj) + sqrt((b / b_adj) * ((b / b_adj) - 1))
}
