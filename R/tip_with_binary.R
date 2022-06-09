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
#' @param effect Numeric positive value. Observed exposure - outcome effect
#'    (assumed to be the exponentiated coefficient, so a relative risk, odds
#'    ratio, or hazard ratio). This can be the point estimate, lower confidence
#'    bound, or upper confidence bound.
#' @param exposed_p Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the exposed population
#' @param unexposed_p Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the unexposed population
#' @param outcome_association Numeric positive value. Estimated association
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
#' ## to estimate the association between an unmeasured confounder and outcome
#' ## needed to tip analysis
#' tip_with_binary(1.2, exposed_p = 0.5, unexposed_p = 0)
#'
#' ## to estimate the number of unmeasured confounders specified needed to tip
#' ## the analysis
#' tip_with_binary(1.2,
#'   exposed_p = 0.5,
#'   unexposed_p = 0,
#'   outcome_association = 1.1)
#'
#' ## Example with broom
#' if (requireNamespace("broom", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE)) {
#'   glm(am ~ mpg, data = mtcars, family = "binomial") %>%
#'    broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
#'    dplyr::filter(term == "mpg") %>%
#'    dplyr::pull(conf.low) %>%
#'    tip_with_binary(exposed_p = 1, outcome_association = 1.15)
#'}
#' @export
tip_with_binary <- function(effect,
                            exposed_p = NULL,
                            unexposed_p = NULL,
                            outcome_association = NULL,
                            verbose = TRUE,
                            correction_factor = "none") {
  exposed_p <- exposed_p %||% list(NULL)
  unexposed_p <- unexposed_p %||% list(NULL)
  outcome_association <- outcome_association %||% list(NULL)

  o <- purrr::pmap(
    list(b = effect,
         exposed_p = exposed_p,
         unexposed_p = unexposed_p,
         outcome_association = outcome_association,
         verbose = verbose,
         correction_factor = correction_factor),
    tip_with_binary_one
  )
  do.call(rbind, o)
}

tip_with_binary_one <- function(b,
                                exposed_p,
                                unexposed_p,
                                outcome_association,
                                verbose,
                                correction_factor) {

  n_unmeasured_confounders <- 1

  correction <- ""
  if (correction_factor == "hr") {
    b <- hr_transform(b)
    outcome_association <- hr_transform(outcome_association)
    correction <- 'You opted to use the hazard ratio correction to convert your hazard ratios to approximate risk ratios.\nThis is a good idea if the outcome is common (>15%).'
  }

  if (correction_factor == "or") {
    b <- or_transform(b)
    outcome_association <- or_transform(outcome_association)
    correction <- 'You opted to use the odds ratio correction to convert your odds ratios to approximate risk ratios.\nThis is a good idea if the outcome is common (>15%).'
  }

  if (is.null(outcome_association)) {
    outcome_association <- tip_gamma(unexposed_p, exposed_p, b)
  } else if (is.null(unexposed_p)) {
    unexposed_p <- tip_p0(exposed_p, outcome_association, b)
  } else if (is.null(exposed_p)) {
    exposed_p <- tip_p1(unexposed_p, outcome_association, b)
  } else {
    n_unmeasured_confounders <-
      tip_n(unexposed_p, exposed_p, outcome_association, b)

    if (any(n_unmeasured_confounders < 0)) {
      if (length(unexposed_p) > 1) {
        unexposed_ps <- unexposed_p[n_unmeasured_confounders < 0]
      } else {
        unexposed_ps <- unexposed_p
      }
      if (length(exposed_p) > 1) {
        exposed_ps <- exposed_p[n_unmeasured_confounders < 0]
      } else {
        exposed_ps <- exposed_p
      }
      if (length(outcome_association) > 1) {
        outcome_associations <- outcome_association[n_unmeasured_confounders < 0]
      } else {
        outcome_associations <- outcome_association
      }

      warning_glue(
        "The observed effect {b} would not tip with the unmeasured confounder given:",
        "\n  * `exposed_p`: {exposed_ps}",
        "\n  * `unexposed_p`: {unexposed_ps}",
        "\n  * `outcome_association`: {outcome_associations}\n\n"
      )
      n_unmeasured_confounders <- max(0, n_unmeasured_confounders)
    }
    too_small <-
      n_unmeasured_confounders < 1 & n_unmeasured_confounders > 0
    if (any(too_small)) {
      if (length(unexposed_p) > 1) {
        unexposed_ps <- unexposed_p[too_small]
      } else {
        unexposed_ps <- unexposed_p
      }
      if (length(exposed_p) > 1) {
        exposed_ps <- exposed_p[too_small]
      } else {
        exposed_ps <- exposed_p
      }
      if (length(outcome_association) > 1) {
        outcome_associations <- outcome_association[too_small]
      } else {
        outcome_associations <- outcome_association
      }
      warning_glue(
        "The observed effect {b} would tip with < 1 of the given unmeasured confounders:",
        "\n  * `exposed_p`: {exposed_ps}",
        "\n  * `unexposed_p`: {unexposed_ps}",
        "\n  * `outcome_association`: {outcome_associations}\n\n"
      )
    }
  }
  o <- tibble::tibble(
    effect_adjusted = 1,
    effect_observed = b,
    exposed_p = exposed_p,
    unexposed_p = unexposed_p,
    outcome_association = outcome_association,
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
        "in the exposed population: {round(o_notip$exposed_p, 2)}\n  * estimated prevalence of ",
        "the unmeasured confounder in the unexposed population: {round(o_notip$unexposed_p, 2)}",
        "\n  * estimated association between the unmeasured confounder and the ",
        "outcome: {round(o_notip$outcome_association, 2)}\n\n{correction}"
      )
    } else if (any(o$n_unmeasured_confounders == 0)) {
      o_notip <- o[o$n_unmeasured_confounders == 0,]
      message_glue(
        "The observed effect ({round(o_notip$effect_observed, 2)}) ",
        "cannot be tipped by an unmeasured confounder\nwith the ",
        "following specifications:",
        "\n  * estimated prevalence of the unmeasured confounder ",
        "in the exposed population: {round(o_notip$exposed_p, 2)}\n  * estimated prevalence of ",
        "the unmeasured confounder in the unexposed population: {round(o_notip$unexposed_p, 2)}",
        "\n  * estimated association between the unmeasured confounder and the ",
        "outcome: {round(o_notip$outcome_association, 2)}\n\n{correction}"
      )

      o_tip <- o[o$n_unmeasured_confounders != 0,]
      message_glue(
        "The observed effect ({round(o_tip$effect_observed, 2)}) WOULD ",
        "be tipped by {round(o_tip$n_unmeasured_confounders)} ",
        "unmeasured confounder{ifelse(o_tip$n_unmeasured_confounders > 1, 's', '')}\n",
        "with the following ",
        "specifications:\n  * estimated prevalence of the unmeasured confounder ",
        "in the exposed population: {round(o_tip$exposed_p, 2)}\n  * estimated prevalence of ",
        "the unmeasured confounder in the unexposed population: {round(o_tip$unexposed_p, 2)}",
        "\n  * estimated association between the unmeasured confounder and the ",
        "outcome: {round(o_tip$outcome_association, 2)}\n\n{correction}"
      )
    } else {
      message_glue(
        "The observed effect ({round(o$effect_observed, 2)}) WOULD ",
        "be tipped by {round(o$n_unmeasured_confounders)} ",
        "unmeasured confounder{ifelse(o$n_unmeasured_confounders > 1, 's', '')}\n",
        "with the following ",
        "specifications:\n  * estimated prevalence of the unmeasured confounder ",
        "in the exposed population: {round(o$exposed_p, 2)}\n  * estimated prevalence of ",
        "the unmeasured confounder in the unexposed population: {round(o$unexposed_p, 2)}",
        "\n  * estimated association between the unmeasured confounder and the ",
        "outcome: {round(o$outcome_association, 2)}\n\n{correction}"
      )
    }
  }
  o
}

#' Tip an observed relative risk with a binary confounder.
#'
#' @description
#' Choose two of the following three to specify, and the third will be estimated:
#' * `exposed_p`
#' * `unexposed_p`
#' * `outcome_association`
#'
#' Alternatively, specify all three and the function will return the number of unmeasured
#' confounders specified needed to tip the analysis.
#' @param effect Numeric positive value. Observed exposure - outcome relative risk.
#'    This can be the point estimate, lower confidence bound, or upper
#'    confidence bound.
#' @param exposed_p Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the exposed population
#' @param unexposed_p Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the unexposed population
#' @param outcome_association Numeric positive value. Estimated association
#'    between the unmeasured confounder and the outcome
#' @param verbose Logical. Indicates whether to print informative message.
#'    Default: `TRUE`

tip_rr_with_binary <- function(effect, exposed_p = NULL, unexposed_p = NULL, outcome_association = NULL, verbose = TRUE) {
  tip_with_binary(effect, exposed_p = exposed_p, unexposed_p = unexposed_p, outcome_association = outcome_association, verbose = verbose)
}

#' Tip an observed hazard ratio with a binary confounder.
#'
#' @description
#' Choose two of the following three to specify, and the third will be estimated:
#' * `exposed_p`
#' * `unexposed_p`
#' * `outcome_association`
#'
#' Alternatively, specify all three and the function will return the number of unmeasured
#' confounders specified needed to tip the analysis.
#' @param effect Numeric positive value. Observed exposure - outcome hazard ratio.
#'    This can be the point estimate, lower confidence bound, or upper
#'    confidence bound.
#' @param exposed_p Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the exposed population
#' @param unexposed_p Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the unexposed population
#' @param outcome_association Numeric positive value. Estimated association
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

tip_hr_with_binary <- function(effect, exposed_p = NULL, unexposed_p = NULL, outcome_association = NULL, verbose = TRUE, hr_correction = FALSE) {
  correction_factor <- ifelse(hr_correction, "hr", "none")
  tip_with_binary(effect, exposed_p = exposed_p, unexposed_p = unexposed_p, outcome_association = outcome_association, verbose = verbose, correction_factor = correction_factor)
}

#' Tip an observed odds ratio with a binary confounder.
#'
#' @description
#' Choose two of the following three to specify, and the third will be estimated:
#' * `exposed_p`
#' * `unexposed_p`
#' * `outcome_association`
#'
#' Alternatively, specify all three and the function will return the number of unmeasured
#' confounders specified needed to tip the analysis.
#' @param effect Numeric positive value. Observed exposure - outcome odds ratio.
#'    This can be the point estimate, lower confidence bound, or upper
#'    confidence bound.
#' @param exposed_p Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the exposed population
#' @param unexposed_p Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the unexposed population
#' @param outcome_association Numeric positive value. Estimated association
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

tip_or_with_binary <- function(effect, exposed_p = NULL, unexposed_p = NULL, outcome_association = NULL, verbose = TRUE, or_correction = FALSE) {
  correction_factor <- ifelse(or_correction, "or", "none")
  tip_with_binary(effect, exposed_p = exposed_p, unexposed_p = unexposed_p, outcome_association = outcome_association, verbose = verbose, correction_factor = correction_factor)
}

#' @rdname tip_with_binary
#' @export
tip_b <- tip_with_binary


