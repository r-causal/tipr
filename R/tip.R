#' Tip a result with a normally distributed confounder.
#'
#' choose one of the following, and the other will be estimated:
#' * `exposure_confounder_effect`
#' * `confounder_outcome_effect`
#'
#' @param effect_observed Numeric positive value. Observed exposure - outcome effect
#'    (assumed to be the exponentiated coefficient, so a relative risk, odds
#'    ratio, or hazard ratio). This can be the point estimate, lower confidence
#'    bound, or upper confidence bound.
#' @param exposure_confounder_effect Numeric. Estimated difference in scaled means between the
#'    unmeasured confounder in the exposed population and unexposed population
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
#'
#' @return Data frame.
#'
#' @examples
#' ## to estimate the relationship between an unmeasured confounder and outcome
#' ## needed to tip analysis
#' tip(1.2, exposure_confounder_effect = -2)
#'
#' ## to estimate the number of unmeasured confounders specified needed to tip
#' ## the analysis
#' tip(1.2, exposure_confounder_effect = -2, confounder_outcome_effect = .99)
#'
#' ## Example with broom
#' if (requireNamespace("broom", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE)) {
#'   glm(am ~ mpg, data = mtcars, family = "binomial") %>%
#'    broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
#'    dplyr::filter(term == "mpg") %>%
#'    dplyr::pull(conf.low) %>%
#'    tip(confounder_outcome_effect = 2.5)
#'}
#' @export
tip <- function(effect_observed, exposure_confounder_effect = NULL, confounder_outcome_effect = NULL,
                verbose = TRUE, correction_factor = "none") {

  exposure_confounder_effect <- exposure_confounder_effect %||% list(NULL)
  confounder_outcome_effect <- confounder_outcome_effect %||% list(NULL)

  o <- purrr::pmap(
    list(b = effect_observed,
         exposure_confounder_effect = exposure_confounder_effect,
         confounder_outcome_effect = confounder_outcome_effect,
         verbose = verbose,
         correction_factor = correction_factor),
    tip_one
  )
  do.call(rbind, o)
}

tip_one <- function(b, exposure_confounder_effect, confounder_outcome_effect, verbose, correction_factor) {
  check_effect(b)
  check_gamma(confounder_outcome_effect)

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

  n_unmeasured_confounders <- 1

  if (is.null(confounder_outcome_effect)) {
    confounder_outcome_effect <- b ^ (1 / exposure_confounder_effect)
  } else if (is.null(exposure_confounder_effect)) {
    exposure_confounder_effect <- log(b) / log(confounder_outcome_effect)
  } else {
    n_unmeasured_confounders <-
      log(b) / (exposure_confounder_effect * log(confounder_outcome_effect))
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
    effect_adjusted = 1,
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
        "outcome: {round(o_notip$confounder_outcome_effect, 2)}\n\n{correction}"
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
        "outcome: {round(o_notip$confounder_outcome_effect, 2)}\n\n{correction}"
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
        "outcome: {round(o_tip$confounder_outcome_effect, 2)}\n\n{correction}"
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
        "outcome: {round(o$confounder_outcome_effect, 2)}\n\n{correction}"
      )
    }
  }
  o
}

#' Tip an observed relative risk with a normally distributed confounder.
#'
#' choose one of the following, and the other will be estimated:
#' * `exposure_confounder_effect`
#' * `confounder_outcome_effect`
#'
#' @param effect_observed Numeric positive value. Observed exposure - outcome
#'    relative risk. This can be the point estimate, lower confidence bound,
#'    or upper confidence bound.
#' @param exposure_confounder_effect Numeric. Estimated difference in scaled means between the
#'    unmeasured confounder in the exposed population and unexposed population
#' @param confounder_outcome_effect Numeric positive value. Estimated relationship
#'    between the unmeasured confounder and the outcome
#' @param verbose Logical. Indicates whether to print informative message.
#'    Default: `TRUE`
#'
#'
#' @return Data frame.
#'
#' @examples
#' ## to estimate the relationship between an unmeasured confounder and outcome
#' ## needed to tip analysis
#' tip_rr(1.2, exposure_confounder_effect = -2)
#'
#' ## to estimate the number of unmeasured confounders specified needed to tip
#' ## the analysis
#' tip_rr(1.2, exposure_confounder_effect = -2, confounder_outcome_effect = .99)
#'
#' @export
tip_rr <- function(effect_observed, exposure_confounder_effect = NULL, confounder_outcome_effect = NULL, verbose = TRUE) {
  tip(effect_observed, exposure_confounder_effect = exposure_confounder_effect, confounder_outcome_effect = confounder_outcome_effect, verbose = verbose)
}


#' Tip an observed hazard ratio with a normally distributed confounder.
#'
#' choose one of the following, and the other will be estimated:
#' * `exposure_confounder_effect`
#' * `confounder_outcome_effect`
#'
#' @param effect_observed Numeric positive value. Observed exposure - outcome hazard ratio.
#'    This can be the point estimate, lower confidence bound, or upper
#'    confidence bound.
#' @param exposure_confounder_effect Numeric. Estimated difference in scaled means between the
#'    unmeasured confounder in the exposed population and unexposed population
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
#'
#' @examples
#' ## to estimate the relationship between an unmeasured confounder and outcome
#' ## needed to tip analysis
#' tip_hr(1.2, exposure_confounder_effect = -2)
#'
#' ## to estimate the number of unmeasured confounders specified needed to tip
#' ## the analysis
#' tip_hr(1.2, exposure_confounder_effect = -2, confounder_outcome_effect = .99)
#'
#' @export
tip_hr <- function(effect_observed, exposure_confounder_effect = NULL, confounder_outcome_effect = NULL, verbose = TRUE, hr_correction = FALSE) {
  correction_factor <- ifelse(hr_correction, "hr", "none")
  tip(effect_observed, exposure_confounder_effect = exposure_confounder_effect, confounder_outcome_effect = confounder_outcome_effect, verbose = verbose, correction_factor = correction_factor)
}

#' Tip an observed odds ratio with a normally distributed confounder.
#'
#' choose one of the following, and the other will be estimated:
#' * `exposure_confounder_effect`
#' * `confounder_outcome_effect`
#'
#' @param effect_observed Numeric positive value. Observed exposure - outcome odds ratio.
#'    This can be the point estimate, lower confidence bound, or upper
#'    confidence bound.
#' @param exposure_confounder_effect Numeric. Estimated difference in scaled means between the
#'    unmeasured confounder in the exposed population and unexposed population
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
#'
#' @examples
#' ## to estimate the relationship between an unmeasured confounder and outcome
#' ## needed to tip analysis
#' tip_or(1.2, exposure_confounder_effect = -2)
#'
#' ## to estimate the number of unmeasured confounders specified needed to tip
#' ## the analysis
#' tip_or(1.2, exposure_confounder_effect = -2, confounder_outcome_effect = .99)
#'
#' ## Example with broom
#' if (requireNamespace("broom", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE)) {
#'   glm(am ~ mpg, data = mtcars, family = "binomial") %>%
#'    broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
#'    dplyr::filter(term == "mpg") %>%
#'    dplyr::pull(conf.low) %>%
#'    tip_or(confounder_outcome_effect = 2.5, or_correction = TRUE)
#'}
#' @export
tip_or <- function(effect_observed, exposure_confounder_effect = NULL, confounder_outcome_effect = NULL, verbose = TRUE, or_correction = FALSE) {
  correction_factor <- ifelse(or_correction, "or", "none")
  tip(effect_observed, exposure_confounder_effect = exposure_confounder_effect, confounder_outcome_effect = confounder_outcome_effect, verbose = verbose, correction_factor = correction_factor)
}

#' @rdname tip_rr
#' @export
tip_rr_with_continuous <- tip_rr

#' @rdname tip_hr
#' @export
tip_hr_with_continuous <- tip_hr

#' @rdname tip_or
#' @export
tip_or_with_continuous <- tip_or

#' @rdname tip
#' @export
tip_with_continuous <- tip

#' @rdname tip
#' @export
tip_c <- tip
