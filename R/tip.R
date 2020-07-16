#' Tip a result with a binary confounder.
#'
#' @description
#' Choose two of the following three to specify, and the third will be estimated:
#' * `exposed_p`
#' * `unexposed_p`
#' * `outcome_association`
#'
#' Alternatively, specify all three and the function will return the number of unmeasured
#' confounders specified needed to tip the analysis.
#'
#' @details [`tip_b()`] is an alias for [`tip_with_binary()`].
#' @param d Data frame. A data frame with the observed effect(s). This should have
#'   at least two columns with the lower and upper confidence bounds. These
#'   columns are assumed to be called `conf.low` and `conf.high`. If this
#'   is not the case, the names can be changed using the `lb_name` and
#'   `ub_name` parameters.
#' @param exposed_p Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the exposed population
#' @param unexposed_p Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the unexposed population
#' @param outcome_association Numeric positive value. estimated association
#'    between the unmeasured confounder and the outcome
#' @param verbose Logical. Indicates whether to print informative message.
#'    Default: `TRUE`
#' @param lb_name Character. Column name of `d` that holds the lower
#'    confidence bound. Default: `conf.low` based on `broom` defaults.
#' @param ub_name Character. Column name of `d` that holds the upper
#'    confidence bound. Default: `conf.high` based on `broom` defaults.
#'
#' @examples
#' d <- data.frame(conf.low = 1.2, conf.high = 1.5)
#' ## to estimate the association between an unmeasured confounder and outcome
#' ## needed to tip analysis
#' tip_with_binary(d, exposed_p = 0.5, unexposed_p = 0)
#'
#' ## to estimate the number of unmeasured confounders specified needed to tip
#' ## the analysis
#' tip_with_binary(d, exposed_p = 0.5, unexposed_p = 0, outcome_association = 1.1)
#'
#' ## Example with broom
#' if (requireNamespace("broom", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE)) {
#'   glm(am ~ mpg, data = mtcars, family = "binomial") %>%
#'    broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
#'    dplyr::filter(term == "mpg") %>%
#'    tip_with_binary(exposed_p = 1, outcome_association = 1.15)
#'}
#' @export
tip_with_binary <- function(d, exposed_p = NULL, unexposed_p = NULL,
                            outcome_association = NULL, verbose = TRUE,
                            lb_name = "conf.low", ub_name = "conf.high") {
  lb <- d[[lb_name]]
  ub <- d[[ub_name]]

  o <- purrr::map2(
    lb, ub,
    ~ tip_with_binary_one(.x, .y,
      exposed_p = exposed_p,
      unexposed_p = unexposed_p,
      outcome_association = outcome_association,
      verbose = verbose
    )
  )
  do.call(rbind, o)
}

tip_with_binary_one <- function(lb, ub, exposed_p, unexposed_p,
                                outcome_association, verbose) {
  b <- get_limiting_bound(lb, ub)

  n_unmeasured_confounders <- 1

  if (is.null(outcome_association)) {
    outcome_association <- tip_gamma(unexposed_p, exposed_p, b)
  } else if (is.null(unexposed_p)) {
    unexposed_p <- tip_p0(exposed_p, outcome_association, b)
  } else if (is.null(exposed_p)) {
    exposed_p <- tip_p1(unexposed_p, outcome_association, b)
  } else {
    n_unmeasured_confounders <- tip_n(unexposed_p, exposed_p, outcome_association, b)
  }
  o <- tibble::tibble(
    observed_lb = lb,
    observed_ub = ub,
    exposed_p = exposed_p,
    unexposed_p = unexposed_p,
    outcome_association = outcome_association,
    n_unmeasured_confounders = n_unmeasured_confounders
  )
  if (verbose) {
    if (o$n_unmeasured_confounders == 0) {
      message_glue(
        "The observed effect ({round(o$observed_lb, 2)}, {round(o$observed_ub, 2)}) ",
        "cannot be tipped by an unmeasured confounder with the ",
        "following specifications:",
        "\n  * estimated prevalence of the unmeasured confounder ",
        "in the exposed population: {round(o$exposed_p, 2)}\n  * estimated prevalence of ",
        "the unmeasured confounder in the unexposed population: {round(o$unexposed_p, 2)}",
        "\n  * estimated association between the unmeasured confounder and the ",
        "outcome: {round(o$outcome_association, 2)}\n"
      )
    } else {
      message_glue(
        "The observed effect ({round(o$observed_lb, 2)}, {round(o$observed_ub, 2)}) WOULD ",
        "be tipped by {round(o$n_unmeasured_confounders)} ",
        "unmeasured confounder{ifelse(o$n_unmeasured_confounders > 1, 's', '')} ",
        "with the following ",
        "specifications:\n  * estimated prevalence of the unmeasured confounder ",
        "in the exposed population: {round(o$exposed_p, 2)}\n  * estimated prevalence of ",
        "the unmeasured confounder in the unexposed population: {round(o$unexposed_p, 2)}",
        "\n  * estimated association between the unmeasured confounder and the ",
        "outcome: {round(o$outcome_association, 2)}\n"
      )
    }
  }
  o
}

#' Tip a result with a continuous confounder.
#'
#' choose one of the following, and the other will be estimated:
#' * `smd`
#' * `gamma`
#'
#' @param d Data frame. A data frame with the observed effect(s). This should have
#'   at least two columns with the lower and upper confidence bounds. These
#'   columns are assumed to be called `conf.low` and `conf.high`. If this
#'   is not the case, the names can be changed using the `lb_name` and
#'   `ub_name` parameters.
#' @param smd Numeric. Estimated standardized mean difference between the
#'    unmeasured confounder in the exposed population and unexposed population
#' @param outcome_association Numeric positive value. estimated association
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
#' tip(d, smd = -2)
#'
#' ## to estimate the number of unmeasured confounders specified needed to tip
#' ## the analysis
#' tip(d, smd = -2, outcome_association = .99)
#'
#' ## Example with broom
#' if (requireNamespace("broom", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE)) {
#'   glm(am ~ mpg, data = mtcars, family = "binomial") %>%
#'    broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
#'    dplyr::filter(term == "mpg") %>%
#'    tip(outcome_association = 2.5)
#'}
#' @export
tip <- function(d, smd = NULL, outcome_association = NULL, verbose = TRUE,
                lb_name = "conf.low", ub_name = "conf.high") {
  lb <- d[[lb_name]]
  ub <- d[[ub_name]]

  o <- purrr::map2(
    lb, ub,
    ~ tip_one(.x, .y,
      smd = smd,
      outcome_association = outcome_association,
      verbose = verbose
    )
  )
  do.call(rbind, o)
}

tip_one <- function(lb, ub, smd, outcome_association, verbose) {
  b <- get_limiting_bound(lb, ub)

  check_gamma(outcome_association)
  n_unmeasured_confounders <- 1
  if (is.null(outcome_association)) {
    outcome_association <- b^(1 / smd)
  } else if (is.null(smd)) {
    smd <- log(b) / log(outcome_association)
  } else {
    n_unmeasured_confounders <- log(b) / (smd * log(outcome_association))
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
        "cannot be tipped by an unmeasured confounder with the ",
        "following specifications:",
        "\n  * estimated standardized mean difference between the ",
        "unmeasured confounder in the exposed population and ",
        "unexposed population: {round(o$smd, 2)}",
        "\n  * estimated association between the unmeasured confounder and the ",
        "outcome: {round(o$outcome_association, 2)}\n"
      )
    } else {
      message_glue(
        "The observed effect ({round(o$observed_lb, 2)}, {round(o$observed_ub, 2)}) WOULD ",
        "be tipped by {round(o$n_unmeasured_confounders)} ",
        "unmeasured confounder{ifelse(o$n_unmeasured_confounders > 1, 's', '')} ",
        "with the following specifications:",
        "\n  * estimated standardized mean difference between the ",
        "unmeasured confounder in the exposed population and ",
        "unexposed population: {round(o$smd, 2)}",
        "\n  * estimated association between the unmeasured confounder and the ",
        "outcome: {round(o$outcome_association, 2)}\n"
      )
    }
  }
  o
}

#' @rdname tip_with_binary
#' @export
tip_b <- tip_with_binary

#' @rdname tip
#' @export
tip_with_continuous <- tip

#' @rdname tip
#' @export
tip_c <- tip
