#' Tip a result with a normally distributed confounder.
#'
#' choose one of the following, and the other will be estimated:
#' * `smd`
#' * `outcome_association`
#'
#' @param effect Numeric positive value. Observed exposure - outcome effect
#'    (assumed to be the exponentiated coefficient, so a relative risk, odds
#'    ratio, or hazard ratio). This can be the point estimate, lower confidence
#'    bound, or upper confidence bound.
#' @param smd Numeric. Estimated difference in scaled means between the
#'    unmeasured confounder in the exposed population and unexposed population
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
#'
#' @return Data frame.
#'
#' @examples
#' ## to estimate the association between an unmeasured confounder and outcome
#' ## needed to tip analysis
#' tip(1.2, smd = -2)
#'
#' ## to estimate the number of unmeasured confounders specified needed to tip
#' ## the analysis
#' tip(1.2, smd = -2, outcome_association = .99)
#'
#' ## Example with broom
#' if (requireNamespace("broom", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE)) {
#'   glm(am ~ mpg, data = mtcars, family = "binomial") %>%
#'    broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
#'    dplyr::filter(term == "mpg") %>%
#'    dplyr::pull(conf.low) %>%
#'    tip(outcome_association = 2.5)
#'}
#' @export
tip <- function(effect, smd = NULL, outcome_association = NULL,
                verbose = TRUE, correction_factor = "none") {

  smd <- smd %||% list(NULL)
  outcome_association <- outcome_association %||% list(NULL)

  o <- purrr::pmap(
    list(b = effect,
         smd = smd,
         outcome_association = outcome_association,
         verbose = verbose,
         correction_factor = correction_factor),
    tip_one
  )
  do.call(rbind, o)
}

tip_one <- function(b, smd, outcome_association, verbose, correction_factor) {
  check_effect(b)
  check_gamma(outcome_association)

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

  n_unmeasured_confounders <- 1

  if (is.null(outcome_association)) {
    outcome_association <- b ^ (1 / smd)
  } else if (is.null(smd)) {
    smd <- log(b) / log(outcome_association)
  } else {
    n_unmeasured_confounders <-
      log(b) / (smd * log(outcome_association))
    if (any(n_unmeasured_confounders < 0)) {
      if (length(smd) > 1) {
        smds <- smd[n_unmeasured_confounders < 0]
      } else {
        smds <- smd
      }
      if (length(outcome_association) > 1) {
        outcome_associations <- outcome_association[n_unmeasured_confounders < 0]
      } else {
        outcome_associations <- outcome_association
      }

      warning_glue(
        "The observed effect {b} would not tip with the unmeasured confounder given:",
        "\n  * `smd`: {smds}",
        "\n  * `outcome_association`: {outcome_associations}\n\n"
      )
      n_unmeasured_confounders <- max(0, n_unmeasured_confounders)
    }
    too_small <-
      n_unmeasured_confounders < 1 & n_unmeasured_confounders > 0
    if (any(too_small)) {
      smds <- ifelse(length(smd) > 1, smd[too_small], smd)
      outcome_associations <-
        ifelse(length(outcome_association) > 1,
               outcome_association[too_small],
               outcome_association)
      warning_glue(
        "The observed effect {b} would tip with < 1 of the given unmeasured confounders:",
        "\n  * `smd`: {smds}",
        "\n  * `outcome_association`: {outcome_associations}\n\n"
      )
    }
  }
  o <- tibble::tibble(
    adjusted_effect = 1,
    observed_effect = b,
    smd = smd,
    outcome_association = outcome_association,
    n_unmeasured_confounders = n_unmeasured_confounders
  )
  if (verbose) {
    if (all(o$n_unmeasured_confounders == 0)) {
      o_notip <- o[o$n_unmeasured_confounders == 0,]
      message_glue(
        "The observed effect ({round(o_notip$observed_effect, 2)}) ",
        "cannot be tipped by an unmeasured confounder\nwith the ",
        "following specifications:",
        "\n  * estimated difference in scaled means between the ",
        "unmeasured confounder\n     in the exposed population and ",
        "unexposed population: {round(o_notip$smd, 2)}",
        "\n  * estimated association between the unmeasured confounder and the ",
        "outcome: {round(o_notip$outcome_association, 2)}\n\n{correction}"
      )
    } else if (any(o$n_unmeasured_confounders == 0)) {
      o_notip <- o[o$n_unmeasured_confounders == 0,]
      message_glue(
        "The observed effect ({round(o_notip$observed_effect, 2)}) ",
        "cannot be tipped by an unmeasured confounder\nwith the ",
        "following specifications:",
        "\n  * estimated difference in scaled means between the ",
        "unmeasured confounder\n     in the exposed population and ",
        "unexposed population: {round(o_notip$smd, 2)}",
        "\n  * estimated association between the unmeasured confounder and the ",
        "outcome: {round(o_notip$outcome_association, 2)}\n\n{correction}"
      )

      o_tip <- o[o$n_unmeasured_confounders != 0,]
      message_glue(
        "The observed effect ({round(o_tip$observed_effect, 2)}) WOULD ",
        "be tipped by {round(o$n_unmeasured_confounders)} ",
        "unmeasured confounder{ifelse(o_tip$n_unmeasured_confounders > 1, 's', '')}\n",
        "with the following specifications:",
        "\n  * estimated difference in scaled means between the ",
        "unmeasured confounder\n    in the exposed population and ",
        "unexposed population: {round(o_tip$smd, 2)}",
        "\n  * estimated association between the unmeasured confounder and the ",
        "outcome: {round(o_tip$outcome_association, 2)}\n\n{correction}"
      )
    } else {
      message_glue(
        "The observed effect ({round(o$observed_effect, 2)}) WOULD ",
        "be tipped by {round(o$n_unmeasured_confounders)} ",
        "unmeasured confounder{ifelse(o$n_unmeasured_confounders > 1, 's', '')}\n",
        "with the following specifications:",
        "\n  * estimated difference in scaled means between the ",
        "unmeasured confounder\n    in the exposed population and ",
        "unexposed population: {round(o$smd, 2)}",
        "\n  * estimated association between the unmeasured confounder and the ",
        "outcome: {round(o$outcome_association, 2)}\n\n{correction}"
      )
    }
  }
  o
}

#' Tip an observed relative risk with a normally distributed confounder.
#'
#' choose one of the following, and the other will be estimated:
#' * `smd`
#' * `outcome_association`
#'
#' @param effect Numeric positive value. Observed exposure - outcome relative risk.
#'    This can be the point estimate, lower confidence bound, or upper
#'    confidence bound.
#' @param smd Numeric. Estimated difference in scaled means between the
#'    unmeasured confounder in the exposed population and unexposed population
#' @param outcome_association Numeric positive value. Estimated association
#'    between the unmeasured confounder and the outcome
#' @param verbose Logical. Indicates whether to print informative message.
#'    Default: `TRUE`
#'
#'
#' @return Data frame.
#'
#' @examples
#' ## to estimate the association between an unmeasured confounder and outcome
#' ## needed to tip analysis
#' tip_rr(1.2, smd = -2)
#'
#' ## to estimate the number of unmeasured confounders specified needed to tip
#' ## the analysis
#' tip_rr(1.2, smd = -2, outcome_association = .99)
#'
#' @export
tip_rr <- function(effect, smd = NULL, outcome_association = NULL, verbose = TRUE) {
  tip(effect, smd = smd, outcome_association = outcome_association, verbose = verbose)
}


#' Tip an observed hazard ratio with a normally distributed confounder.
#'
#' choose one of the following, and the other will be estimated:
#' * `smd`
#' * `outcome_association`
#'
#' @param effect Numeric positive value. Observed exposure - outcome hazard ratio.
#'    This can be the point estimate, lower confidence bound, or upper
#'    confidence bound.
#' @param smd Numeric. Estimated difference in scaled means between the
#'    unmeasured confounder in the exposed population and unexposed population
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
#'
#' @examples
#' ## to estimate the association between an unmeasured confounder and outcome
#' ## needed to tip analysis
#' tip_hr(1.2, smd = -2)
#'
#' ## to estimate the number of unmeasured confounders specified needed to tip
#' ## the analysis
#' tip_hr(1.2, smd = -2, outcome_association = .99)
#'
#' @export
tip_hr <- function(effect, smd = NULL, outcome_association = NULL, verbose = TRUE, hr_correction = FALSE) {
  correction_factor <- ifelse(hr_correction, "hr", "none")
  tip(effect, smd = smd, outcome_association = outcome_association, verbose = verbose, correction_factor = correction_factor)
}

#' Tip an observed odds ratio with a normally distributed confounder.
#'
#' choose one of the following, and the other will be estimated:
#' * `smd`
#' * `outcome_association`
#'
#' @param effect Numeric positive value. Observed exposure - outcome odds ratio.
#'    This can be the point estimate, lower confidence bound, or upper
#'    confidence bound.
#' @param smd Numeric. Estimated difference in scaled means between the
#'    unmeasured confounder in the exposed population and unexposed population
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
#'
#' @examples
#' ## to estimate the association between an unmeasured confounder and outcome
#' ## needed to tip analysis
#' tip_or(1.2, smd = -2)
#'
#' ## to estimate the number of unmeasured confounders specified needed to tip
#' ## the analysis
#' tip_or(1.2, smd = -2, outcome_association = .99)
#'
#' ## Example with broom
#' if (requireNamespace("broom", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE)) {
#'   glm(am ~ mpg, data = mtcars, family = "binomial") %>%
#'    broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
#'    dplyr::filter(term == "mpg") %>%
#'    dplyr::pull(conf.low) %>%
#'    tip_or(outcome_association = 2.5, or_correction = TRUE)
#'}
#' @export
tip_or <- function(effect, smd = NULL, outcome_association = NULL, verbose = TRUE, or_correction = FALSE) {
  correction_factor <- ifelse(or_correction, "or", "none")
  tip(effect, smd = smd, outcome_association = outcome_association, verbose = verbose, correction_factor = correction_factor)
}


#' @rdname tip
#' @export
tip_with_continuous <- tip

#' @rdname tip
#' @export
tip_c <- tip
