#' Create a data frame to assist with creating an observed bias plot
#'
#' @param ps_mod Model object for the propensity score model
#' @param outcome_mod Model object for the outcome model
#' @param groups Named list of groups of covariates to drop together
#'
#' @return Data frame with the following columns:
#'   * `dropped`: The covariate or group of covariates that were dropped
#'   * `group`: An indicator for whether `dropped` refers to a single covariate or a group of covariates
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
#'  groups = list(
#'    group_one = c("mpg", "hp"),
#'    group_two = c("cyl", "wt")
#'    )
#'  )
observed_bias_tbl <- function(ps_mod, outcome_mod, groups = NULL) {
  check_groups_list(groups)

  exposure <- get_y(ps_mod)
  outcome <- get_y(outcome_mod)
  d <- drop_cov_tbl(ps_mod, outcome_mod, groups = groups)
  d <- add_formula(d, exposure, outcome)
  observed_bias_tbl <- tibble::tibble(
    dropped = ifelse(
      is.na(d$ps_drop_clean),
      d$outcome_drop_clean,
      d$ps_drop_clean
    ),
    group = d$group,
    ps_formula = d$ps_form,
    outcome_formula = d$outcome_form,
    ps_model = purrr::map2(d$ps_drop_clean, d$ps_form, ~ update_model(.x, .y, ps_mod))
  )
  tibble::add_column(
    observed_bias_tbl,
    p = purrr::map(observed_bias_tbl$ps_model, predict, type = "response")
  )
}
