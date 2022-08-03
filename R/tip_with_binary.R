#' Tip a result with a binary confounder.
#'
#' @description
#' Choose two of the following three to specify, and the third will be estimated:
#' * `exposed_confounder_prev`
#' * `unexposed_confounder_prev`
#' * `confounder_outcome_effect`
#'
#' Alternatively, specify all three and the function will return the number of unmeasured
#' confounders specified needed to tip the analysis.
#'
#' @details [`tip_b()`] is an alias for [`tip_with_binary()`].
#' @param effect_observed Numeric positive value. Observed exposure - outcome effect
#'    (assumed to be the exponentiated coefficient, so a relative risk, odds
#'    ratio, or hazard ratio). This can be the point estimate, lower confidence
#'    bound, or upper confidence bound.
#' @param exposed_confounder_prev Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the exposed population
#' @param unexposed_confounder_prev Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the unexposed population
#' @param confounder_outcome_effect Numeric positive value. Estimated relationship
#'    between the unmeasured confounder and the outcome
#' @param verbose Logical. Indicates whether to print informative message.
#'    Default: `TRUE`
#' @param correction_factor Character string. Options are "none", "hr", "or".
#'   For common outcomes (>15%), the odds ratio or hazard ratio is not a good
#'   estimate for the relative risk. In these cases, we can apply a correction
#'   factor. If you are supplying a hazard ratio for a common outcome, set
#'   this to "hr"; if you are supplying an odds ratio for a common outcome, set
#'   this to "or"; if you are supplying a risk ratio or your outcome is rare,
#'   set this to "none" (default).
#'
#' @examples
#' ## to estimate the relationship between an unmeasured confounder and outcome
#' ## needed to tip analysis
#' tip_with_binary(1.2, exposed_confounder_prev = 0.5, unexposed_confounder_prev = 0)
#'
#' ## to estimate the number of unmeasured confounders specified needed to tip
#' ## the analysis
#' tip_with_binary(1.2,
#'   exposed_confounder_prev = 0.5,
#'   unexposed_confounder_prev = 0,
#'   confounder_outcome_effect = 1.1)
#'
#' ## Example with broom
#' if (requireNamespace("broom", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE)) {
#'   glm(am ~ mpg, data = mtcars, family = "binomial") %>%
#'    broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
#'    dplyr::filter(term == "mpg") %>%
#'    dplyr::pull(conf.low) %>%
#'    tip_with_binary(exposed_confounder_prev = 1, confounder_outcome_effect = 1.15)
#'}
#' @export
tip_with_binary <- function(effect_observed,
                            exposed_confounder_prev = NULL,
                            unexposed_confounder_prev = NULL,
                            confounder_outcome_effect = NULL,
                            verbose = TRUE,
                            correction_factor = "none") {
  exposed_confounder_prev <- exposed_confounder_prev %||% list(NULL)
  unexposed_confounder_prev <- unexposed_confounder_prev %||% list(NULL)
  confounder_outcome_effect <- confounder_outcome_effect %||% list(NULL)

  o <- purrr::pmap(
    list(b = effect_observed,
         exposed_confounder_prev = exposed_confounder_prev,
         unexposed_confounder_prev = unexposed_confounder_prev,
         confounder_outcome_effect = confounder_outcome_effect,
         verbose = verbose,
         correction_factor = correction_factor),
    tip_with_binary_one
  )
  do.call(rbind, o)
}

tip_with_binary_one <- function(b,
                                exposed_confounder_prev,
                                unexposed_confounder_prev,
                                confounder_outcome_effect,
                                verbose,
                                correction_factor) {

  n_unmeasured_confounders <- 1

  correction <- ""
  if (correction_factor == "hr") {
    b <- hr_transform(b)
    confounder_outcome_effect <- hr_transform(confounder_outcome_effect)
    correction <- 'You opted to use the hazard ratio correction to convert your hazard ratios to approximate risk ratios.\nThis is a good idea if the outcome is common (>15%).'
  }

  if (correction_factor == "or") {
    b <- or_transform(b)
    confounder_outcome_effect <- or_transform(confounder_outcome_effect)
    correction <- 'You opted to use the odds ratio correction to convert your odds ratios to approximate risk ratios.\nThis is a good idea if the outcome is common (>15%).'
  }

  if (is.null(confounder_outcome_effect)) {
    confounder_outcome_effect <- tip_gamma(unexposed_confounder_prev, exposed_confounder_prev, b)
  } else if (is.null(unexposed_confounder_prev)) {
    unexposed_confounder_prev <- tip_p0(exposed_confounder_prev, confounder_outcome_effect, b)
  } else if (is.null(exposed_confounder_prev)) {
    exposed_confounder_prev <- tip_p1(unexposed_confounder_prev, confounder_outcome_effect, b)
  } else {
    n_unmeasured_confounders <-
      tip_n(unexposed_confounder_prev, exposed_confounder_prev, confounder_outcome_effect, b)

    if (any(n_unmeasured_confounders < 0)) {
      if (length(unexposed_confounder_prev) > 1) {
        unexposed_confounder_prevs <- unexposed_confounder_prev[n_unmeasured_confounders < 0]
      } else {
        unexposed_confounder_prevs <- unexposed_confounder_prev
      }
      if (length(exposed_confounder_prev) > 1) {
        exposed_confounder_prevs <- exposed_confounder_prev[n_unmeasured_confounders < 0]
      } else {
        exposed_confounder_prevs <- exposed_confounder_prev
      }
      if (length(confounder_outcome_effect) > 1) {
        confounder_outcome_effects <- confounder_outcome_effect[n_unmeasured_confounders < 0]
      } else {
        confounder_outcome_effects <- confounder_outcome_effect
      }

      warning_glue(
        "The observed effect {b} would not tip with the unmeasured confounder given:",
        "\n  * `exposed_confounder_prev`: {exposed_confounder_prevs}",
        "\n  * `unexposed_confounder_prev`: {unexposed_confounder_prevs}",
        "\n  * `confounder_outcome_effect`: {confounder_outcome_effects}\n\n"
      )
      n_unmeasured_confounders <- max(0, n_unmeasured_confounders)
    }
    too_small <-
      n_unmeasured_confounders < 1 & n_unmeasured_confounders > 0
    if (any(too_small)) {
      if (length(unexposed_confounder_prev) > 1) {
        unexposed_confounder_prevs <- unexposed_confounder_prev[too_small]
      } else {
        unexposed_confounder_prevs <- unexposed_confounder_prev
      }
      if (length(exposed_confounder_prev) > 1) {
        exposed_confounder_prevs <- exposed_confounder_prev[too_small]
      } else {
        exposed_confounder_prevs <- exposed_confounder_prev
      }
      if (length(confounder_outcome_effect) > 1) {
        confounder_outcome_effects <- confounder_outcome_effect[too_small]
      } else {
        confounder_outcome_effects <- confounder_outcome_effect
      }
      warning_glue(
        "The observed effect {b} would tip with < 1 of the given unmeasured confounders:",
        "\n  * `exposed_confounder_prev`: {exposed_confounder_prevs}",
        "\n  * `unexposed_confounder_prev`: {unexposed_confounder_prevs}",
        "\n  * `confounder_outcome_effect`: {confounder_outcome_effects}\n\n"
      )
    }
  }
  o <- tibble::tibble(
    effect_adjusted = 1,
    effect_observed = b,
    exposed_confounder_prev = exposed_confounder_prev,
    unexposed_confounder_prev = unexposed_confounder_prev,
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
        "\n  * estimated prevalence of the unmeasured confounder ",
        "in the exposed population: {round(o_notip$exposed_confounder_prev, 2)}\n  * estimated prevalence of ",
        "the unmeasured confounder in the unexposed population: {round(o_notip$unexposed_confounder_prev, 2)}",
        "\n  * estimated relationship between the unmeasured confounder and the ",
        "outcome: {round(o_notip$confounder_outcome_effect, 2)}\n\n{correction}"
      )
    } else if (any(o$n_unmeasured_confounders == 0)) {
      o_notip <- o[o$n_unmeasured_confounders == 0,]
      message_glue(
        "The observed effect ({round(o_notip$effect_observed, 2)}) ",
        "cannot be tipped by an unmeasured confounder\nwith the ",
        "following specifications:",
        "\n  * estimated prevalence of the unmeasured confounder ",
        "in the exposed population: {round(o_notip$exposed_confounder_prev, 2)}\n  * estimated prevalence of ",
        "the unmeasured confounder in the unexposed population: {round(o_notip$unexposed_confounder_prev, 2)}",
        "\n  * estimated relationship between the unmeasured confounder and the ",
        "outcome: {round(o_notip$confounder_outcome_effect, 2)}\n\n{correction}"
      )

      o_tip <- o[o$n_unmeasured_confounders != 0,]
      message_glue(
        "The observed effect ({round(o_tip$effect_observed, 2)}) WOULD ",
        "be tipped by {round(o_tip$n_unmeasured_confounders)} ",
        "unmeasured confounder{ifelse(o_tip$n_unmeasured_confounders > 1, 's', '')}\n",
        "with the following ",
        "specifications:\n  * estimated prevalence of the unmeasured confounder ",
        "in the exposed population: {round(o_tip$exposed_confounder_prev, 2)}\n  * estimated prevalence of ",
        "the unmeasured confounder in the unexposed population: {round(o_tip$unexposed_confounder_prev, 2)}",
        "\n  * estimated relationship between the unmeasured confounder and the ",
        "outcome: {round(o_tip$confounder_outcome_effect, 2)}\n\n{correction}"
      )
    } else {
      message_glue(
        "The observed effect ({round(o$effect_observed, 2)}) WOULD ",
        "be tipped by {round(o$n_unmeasured_confounders)} ",
        "unmeasured confounder{ifelse(o$n_unmeasured_confounders > 1, 's', '')}\n",
        "with the following ",
        "specifications:\n  * estimated prevalence of the unmeasured confounder ",
        "in the exposed population: {round(o$exposed_confounder_prev, 2)}\n  * estimated prevalence of ",
        "the unmeasured confounder in the unexposed population: {round(o$unexposed_confounder_prev, 2)}",
        "\n  * estimated relationship between the unmeasured confounder and the ",
        "outcome: {round(o$confounder_outcome_effect, 2)}\n\n{correction}"
      )
    }
  }
  o
}

#' Tip an observed relative risk with a binary confounder.
#'
#' @description
#' Choose two of the following three to specify, and the third will be estimated:
#' * `exposed_confounder_prev`
#' * `unexposed_confounder_prev`
#' * `confounder_outcome_effect`
#'
#' Alternatively, specify all three and the function will return the number of unmeasured
#' confounders specified needed to tip the analysis.
#' @param effect_observed Numeric positive value. Observed exposure - outcome relative risk.
#'    This can be the point estimate, lower confidence bound, or upper
#'    confidence bound.
#' @param exposed_confounder_prev Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the exposed population
#' @param unexposed_confounder_prev Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the unexposed population
#' @param confounder_outcome_effect Numeric positive value. Estimated relationship
#'    between the unmeasured confounder and the outcome
#' @param verbose Logical. Indicates whether to print informative message.
#'    Default: `TRUE`

tip_rr_with_binary <- function(effect_observed, exposed_confounder_prev = NULL, unexposed_confounder_prev = NULL, confounder_outcome_effect = NULL, verbose = TRUE) {
  tip_with_binary(effect_observed, exposed_confounder_prev = exposed_confounder_prev, unexposed_confounder_prev = unexposed_confounder_prev, confounder_outcome_effect = confounder_outcome_effect, verbose = verbose)
}

#' Tip an observed hazard ratio with a binary confounder.
#'
#' @description
#' Choose two of the following three to specify, and the third will be estimated:
#' * `exposed_confounder_prev`
#' * `unexposed_confounder_prev`
#' * `confounder_outcome_effect`
#'
#' Alternatively, specify all three and the function will return the number of unmeasured
#' confounders specified needed to tip the analysis.
#' @param effect_observed Numeric positive value. Observed exposure - outcome hazard ratio.
#'    This can be the point estimate, lower confidence bound, or upper
#'    confidence bound.
#' @param exposed_confounder_prev Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the exposed population
#' @param unexposed_confounder_prev Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the unexposed population
#' @param confounder_outcome_effect Numeric positive value. Estimated relationship
#'    between the unmeasured confounder and the outcome
#' @param verbose Logical. Indicates whether to print informative message.
#'    Default: `TRUE`
#' @param hr_correction Logical. Indicates whether to use a correction factor.
#'    The methods used for this function are based on relative risks. For rare
#'    outcomes, a hazard ratio approximates a relative risk. For common outcomes,
#'    a correction factor is needed. If you have a common outcome (>15%),
#'    set this to `TRUE`. Default: `FALSE`.
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' tip_hr_with_binary(0.9, 0.9, 0.1)

tip_hr_with_binary <- function(effect_observed, exposed_confounder_prev = NULL, unexposed_confounder_prev = NULL, confounder_outcome_effect = NULL, verbose = TRUE, hr_correction = FALSE) {
  correction_factor <- ifelse(hr_correction, "hr", "none")
  tip_with_binary(effect_observed, exposed_confounder_prev = exposed_confounder_prev, unexposed_confounder_prev = unexposed_confounder_prev, confounder_outcome_effect = confounder_outcome_effect, verbose = verbose, correction_factor = correction_factor)
}

#' Tip an observed odds ratio with a binary confounder.
#'
#' @description
#' Choose two of the following three to specify, and the third will be estimated:
#' * `exposed_confounder_prev`
#' * `unexposed_confounder_prev`
#' * `confounder_outcome_effect`
#'
#' Alternatively, specify all three and the function will return the number of unmeasured
#' confounders specified needed to tip the analysis.
#' @param effect_observed Numeric positive value. Observed exposure - outcome odds ratio.
#'    This can be the point estimate, lower confidence bound, or upper
#'    confidence bound.
#' @param exposed_confounder_prev Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the exposed population
#' @param unexposed_confounder_prev Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the unexposed population
#' @param confounder_outcome_effect Numeric positive value. Estimated relationship
#'    between the unmeasured confounder and the outcome
#' @param verbose Logical. Indicates whether to print informative message.
#'    Default: `TRUE`
#' @param or_correction Logical. Indicates whether to use a correction factor.
#'    The methods used for this function are based on relative risks. For rare
#'    outcomes, an odds ratio approximates a relative risk. For common outcomes,
#'    a correction factor is needed. If you have a common outcome (>15%),
#'    set this to `TRUE`. Default: `FALSE`.
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' tip_or_with_binary(0.9, 0.9, 0.1)

tip_or_with_binary <- function(effect_observed, exposed_confounder_prev = NULL, unexposed_confounder_prev = NULL, confounder_outcome_effect = NULL, verbose = TRUE, or_correction = FALSE) {
  correction_factor <- ifelse(or_correction, "or", "none")
  tip_with_binary(effect_observed, exposed_confounder_prev = exposed_confounder_prev, unexposed_confounder_prev = unexposed_confounder_prev, confounder_outcome_effect = confounder_outcome_effect, verbose = verbose, correction_factor = correction_factor)
}

#' @rdname tip_with_binary
#' @export
tip_b <- tip_with_binary


