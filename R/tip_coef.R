#' Tip a linear model coefficient with a continuous confounder.
#'
#' choose one of the following, and the other will be estimated:
#' * `exposure_confounder_effect`
#' * `confounder_outcome_effect`
#'
#' @param effect_observed Numeric. Observed exposure - outcome effect from
#'    a regression model. This can be the beta coefficient, the lower
#'    confidence bound of the beta coefficient, or the upper confidence bound
#'    of the beta coefficient.
#' @param exposure_confounder_effect Numeric. Estimated scaled mean difference
#'    between the unmeasured confounder in the exposed population and unexposed
#'    population
#' @param confounder_outcome_effect Numeric positive value. Estimated relationship
#'    between the unmeasured confounder and the outcome
#' @param verbose Logical. Indicates whether to print informative message.
#'    Default: `TRUE`
#'
#' @return Data frame.
#'
#' @examples
#' ## to estimate the relationship between an unmeasured confounder and outcome
#' ## needed to tip analysis
#' tip_coef(1.2, exposure_confounder_effect = -2)
#'
#' ## to estimate the number of unmeasured confounders specified needed to tip
#' ## the analysis
#' tip_coef(1.2, exposure_confounder_effect = -2, confounder_outcome_effect = -0.05)
#'
#' ## Example with broom
#' if (requireNamespace("broom", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE)) {
#'   lm(wt ~ mpg, data = mtcars) %>%
#'    broom::tidy(conf.int = TRUE) %>%
#'    dplyr::filter(term == "mpg") %>%
#'    dplyr::pull(conf.low) %>%
#'    tip_coef(confounder_outcome_effect = 2.5)
#'}
#' @export
tip_coef <- function(effect_observed, exposure_confounder_effect = NULL, confounder_outcome_effect = NULL, verbose = TRUE) {

  o <- purrr::map(
    effect_observed,
    ~ tip_coef_one(.x,
                   exposure_confounder_effect = exposure_confounder_effect,
                   confounder_outcome_effect = confounder_outcome_effect,
                   verbose = verbose
    )
  )
  do.call(rbind, o)
}

tip_coef_one <- function(b, exposure_confounder_effect, confounder_outcome_effect, verbose) {

  n_unmeasured_confounders <- 1

  if (is.null(confounder_outcome_effect)) {
    confounder_outcome_effect <- b / exposure_confounder_effect
  } else if (is.null(exposure_confounder_effect)) {
    exposure_confounder_effect <- b / confounder_outcome_effect
  } else {
    n_unmeasured_confounders <- b / (exposure_confounder_effect * confounder_outcome_effect)
    if (any(n_unmeasured_confounders < 0)) {
      if (length(exposure_confounder_effect) > 1) {
        exposure_confounder_effects <- exposure_confounder_effect[n_unmeasured_confounders < 0]
      } else {
        exposure_confounder_effects <- exposure_confounder_effect
      }
      if (length(confounder_outcome_effect) > 1) {
        confounder_outcome_effects <- confounder_outcome_effect[n_unmeasured_confounders < 0]
      } else {
        confounder_outcome_effects <- confounder_outcome_effect
      }

      warning_glue(
        "The observed effect {b} would not tip with the unmeasured confounder given:",
        "\n  * `exposure_confounder_effect`: {exposure_confounder_effects}",
        "\n  * `confounder_outcome_effect`: {confounder_outcome_effects}\n\n"
      )
      n_unmeasured_confounders <- max(0, n_unmeasured_confounders)
    }
    too_small <-
      n_unmeasured_confounders < 1 & n_unmeasured_confounders > 0
    if (any(too_small)) {
      exposure_confounder_effects <- ifelse(length(exposure_confounder_effect) > 1, exposure_confounder_effect[too_small], exposure_confounder_effect)
      confounder_outcome_effects <-
        ifelse(length(confounder_outcome_effect) > 1,
               confounder_outcome_effect[too_small],
               confounder_outcome_effect)
      warning_glue(
        "The observed effect {b} would tip with < 1 of the given unmeasured confounders:",
        "\n  * `exposure_confounder_effect`: {exposure_confounder_effects}",
        "\n  * `confounder_outcome_effect`: {confounder_outcome_effects}\n\n"
      )
    }
  }
  o <- tibble::tibble(
    effect_observed = b,
    exposure_confounder_effect = exposure_confounder_effect,
    confounder_outcome_effect = confounder_outcome_effect,
    n_unmeasured_confounders = n_unmeasured_confounders
  )
  if (verbose) {
    if (all(o$n_unmeasured_confounders == 0)) {
      o_notip <- o[o$n_unmeasured_confounders == 0,]
      message_glue(
        "The observed effect ({round(o_notip$effect_observed, 2)}) ",
        "cannot be tipped by an unmeasured confounder\nwith the ",
        "following specifications:",
        "\n  * estimated difference in scaled means between the ",
        "unmeasured confounder\n     in the exposed population and ",
        "unexposed population: {round(o_notip$exposure_confounder_effect, 2)}",
        "\n  * estimated relationship between the unmeasured confounder and the ",
        "outcome: {round(o_notip$confounder_outcome_effect, 2)}\n\n"
      )
    } else if (any(o$n_unmeasured_confounders == 0)) {
      o_notip <- o[o$n_unmeasured_confounders == 0,]
      message_glue(
        "The observed effect ({round(o_notip$effect_observed, 2)}) ",
        "cannot be tipped by an unmeasured confounder\nwith the ",
        "following specifications:",
        "\n  * estimated difference in scaled means between the ",
        "unmeasured confounder\n     in the exposed population and ",
        "unexposed population: {round(o_notip$exposure_confounder_effect, 2)}",
        "\n  * estimated relationship between the unmeasured confounder and the ",
        "outcome: {round(o_notip$confounder_outcome_effect, 2)}\n\n"
      )

      o_tip <- o[o$n_unmeasured_confounders != 0,]
      message_glue(
        "The observed effect ({round(o_tip$effect_observed, 2)}) WOULD ",
        "be tipped by {round(o$n_unmeasured_confounders)} ",
        "unmeasured confounder{ifelse(o_tip$n_unmeasured_confounders > 1, 's', '')}\n",
        "with the following specifications:",
        "\n  * estimated difference in scaled means between the ",
        "unmeasured confounder\n    in the exposed population and ",
        "unexposed population: {round(o_tip$exposure_confounder_effect, 2)}",
        "\n  * estimated relationship between the unmeasured confounder and the ",
        "outcome: {round(o_tip$confounder_outcome_effect, 2)}\n\n"
      )
    } else {
      message_glue(
        "The observed effect ({round(o$effect_observed, 2)}) WOULD ",
        "be tipped by {round(o$n_unmeasured_confounders)} ",
        "unmeasured confounder{ifelse(o$n_unmeasured_confounders > 1, 's', '')}\n",
        "with the following specifications:",
        "\n  * estimated difference in scaled means between the ",
        "unmeasured confounder\n    in the exposed population and ",
        "unexposed population: {round(o$exposure_confounder_effect, 2)}",
        "\n  * estimated relationship between the unmeasured confounder and the ",
        "outcome: {round(o$confounder_outcome_effect, 2)}\n\n"
      )
    }
  }
  o
}


#' @rdname tip_coef
#' @export
tip_coef_with_continuous <- tip_coef
