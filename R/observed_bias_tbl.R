#' Create a data frame to assist with creating an observed bias plot
#'
#' @param ps_mod Model object for the propensity score model
#' @param outcome_mod Model object for the outcome model
#' @param drop_list Named list of covariates or groups of covariates to drop if
#'   `NULL`, will default to dropping each covariate one at a time.
#'
#' @return Data frame with the following columns:
#'   * `dropped`: The covariate or group of covariates that were dropped
#'   * `type`: Explanation of `dropped`, whether it refers to a single covariate (`covariate`) or a group of covariates (`group`)
#'   * `ps_formula`: The new formula for the updated propensity score model
#'   * `outcome_formula`: The new formula for the updated outcome model
#'   * `ps_model`: The new model object for the updated propensity score model
#'   * `p`: The updated propensity score
#' @export
#'
#' @examples
#' ps_mod <- glm(am ~ mpg + cyl + I(hp^2), data = mtcars)
#' outcome_mod <- lm(qsec ~ am + hp + disp + wt, data = mtcars)
#' observed_bias_tbl(
#'  ps_mod,
#'  outcome_mod,
#'  drop_list = list(
#'    group_one = c("mpg", "hp"),
#'    group_two = c("cyl", "wt")
#'    )
#'  )

observed_bias_tbl <- function(ps_mod, outcome_mod, drop_list = NULL) {
  c <- create_covariate_lists(ps_mod, outcome_mod)

  if (is.null(drop_list)) {
    drop_list <- create_individual_covariate_list(c)
  }

  check_drop_list(drop_list)
  outcome <- get_y(outcome_mod)

  g <- drop_tbl(drop_list, c)
  d <- add_formula(g, c[["exposure"]], outcome)

  observed_bias_tbl <- tibble::tibble(
    dropped = d$dropped,
    type = d$type,
    ps_formula = d$ps_form,
    outcome_formula = d$outcome_form,
    ps_model = purrr::map(d$ps_form, ~ stats::update(ps_mod, .x))
  )

  tibble::add_column(
    observed_bias_tbl,
    p = purrr::map(observed_bias_tbl$ps_model,
                   stats::predict, type = "response")
  )
}
