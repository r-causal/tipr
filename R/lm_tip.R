#' Tip a linear model result with a continuous confounder.
#'
#' choose one of the following, and the other will be estimated:
#' * `smd`
#' * `outcome_association`
#'
#' @param d Data frame. A data frame with the observed effect(s). This should have
#'   at least two columns with the lower and upper confidence bounds. These
#'   columns are assumed to be called `conf.low` and `conf.high`. If this
#'   is not the case, the names can be changed using the `lb_name` and
#'   `ub_name` parameters.
#' @param smd Numeric. Estimated scaled mean difference between the
#'    unmeasured confounder in the exposed population and unexposed population
#' @param outcome_association Numeric positive value. Estimated association
#'    between the unmeasured confounder and the outcome
#' @param verbose Logical. Indicates whether to print informative message.
#'    Default: `TRUE`
#' @param lb_name Character. Column name of `d` that holds the lower
#'    confidence bound. Default: `conf.low` based on `broom` defaults.
#' @param ub_name Character. Column name of `d` that holds the upper
#'    confidence bound. Default: `conf.high` based on `broom` defaults.
#'
#' @return Data frame.
#'
#' @examples
#' d <- data.frame(conf.low = 1.2, conf.high = 1.5)
#' ## to estimate the association between an unmeasured confounder and outcome
#' ## needed to tip analysis
#' lm_tip(d, smd = -2)
#'
#' ## to estimate the number of unmeasured confounders specified needed to tip
#' ## the analysis
#' lm_tip(d, smd = -2, outcome_association = -0.05)
#'
#' ## Example with broom
#' if (requireNamespace("broom", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE)) {
#'   lm(wt ~ mpg, data = mtcars) %>%
#'    broom::tidy(conf.int = TRUE) %>%
#'    dplyr::filter(term == "mpg") %>%
#'    lm_tip(outcome_association = 2.5)
#'}
#' @export
lm_tip <- function(d, smd = NULL, outcome_association = NULL, verbose = TRUE,
                lb_name = "conf.low", ub_name = "conf.high") {
  lb <- d[[lb_name]]
  ub <- d[[ub_name]]

  o <- purrr::map2(
    lb, ub,
    ~ lm_tip_one(.x, .y,
              smd = smd,
              outcome_association = outcome_association,
              verbose = verbose
    )
  )
  do.call(rbind, o)
}

lm_tip_one <- function(lb, ub, smd, outcome_association, verbose) {
  b <- get_lm_limiting_bound(lb, ub)

  n_unmeasured_confounders <- 1
  if (is.null(outcome_association)) {
    outcome_association <- b / smd
  } else if (is.null(smd)) {
    smd <- b / outcome_association
  } else {
    n_unmeasured_confounders <- b / (smd * outcome_association)
    if (n_unmeasured_confounders < 0) {
      n_unmeasured_confounders <- 0
      warning_glue("The limiting bound {b} would not tip with the unmeasured confounder given.")
    } else if (n_unmeasured_confounders < 1) {
      warning_glue("The limiting bound {b} would tip with < 1 of the given unmeasured confounders.")
    }
  }
  o <- tibble::tibble(
    observed_lb = lb,
    observed_ub = ub,
    smd = smd,
    outcome_association = outcome_association,
    n_unmeasured_confounders = n_unmeasured_confounders
  )
  if (verbose) {
    if (o$n_unmeasured_confounders == 0) {
      message_glue(
        "The observed effect ({round(o$observed_lb, 2)}, {round(o$observed_ub, 2)}) ",
        "cannot be tipped by an unmeasured confounder\nwith the ",
        "following specifications:",
        "\n  * estimated standardized mean difference between the ",
        "unmeasured confounder\n     in the exposed population and ",
        "unexposed population: {round(o$smd, 2)}",
        "\n  * estimated association between the unmeasured confounder and the ",
        "outcome: {round(o$outcome_association, 2)}\n"
      )
    } else {
      message_glue(
        "The observed effect ({round(o$observed_lb, 2)}, {round(o$observed_ub, 2)}) WOULD ",
        "be tipped by {round(o$n_unmeasured_confounders)} ",
        "unmeasured confounder{ifelse(o$n_unmeasured_confounders > 1, 's', '')}\n",
        "with the following specifications:",
        "\n  * estimated standardized mean difference between the ",
        "unmeasured confounder\n    in the exposed population and ",
        "unexposed population: {round(o$smd, 2)}",
        "\n  * estimated association between the unmeasured confounder and the ",
        "outcome: {round(o$outcome_association, 2)}\n"
      )
    }
  }
  o
}
