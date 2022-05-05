#' Adjust an observed regression coefficient for a normally distributed
#' confounder
#'
#' @param effect Numeric. Observed exposure - outcome effect from a regression
#'    model. This can be the beta coefficient, the lower confidence bound of
#'    the beta coefficient, or the upper confidence bound of the beta
#'    coefficient.
#' @param smd Numeric. Estimated difference in scaled means between the
#'    unmeasured confounder in the exposed population and unexposed population
#' @param outcome_association Numeric. Estimated association
#'    between the unmeasured confounder and the outcome.
#' @param verbose Logical. Indicates whether to print informative message.
#'    Default: `TRUE`
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' ## Update an observed coefficient of 0.5 with an unmeasured confounder
#' ## with a difference in scaled means between exposure groups of 0.2
#' ## and coefficient of 0.3
#' adjust_coef(0.5, 0.2, 0.3)
adjust_coef <- function(effect, smd, outcome_association, verbose = TRUE) {
  effect_adj <- effect - outcome_association * smd
  o <- tibble::tibble(
    effect_adjusted = effect_adj,
    effect_observed = effect,
    smd = smd,
    outcome_association = outcome_association
  )
  if (verbose) {
        message_glue(
          "The observed effect ({round(effect, 2)}) ",
          "is updated to {round(effect_adj, 2)} ",
          "by a confounder with the following specifications:",
          "\n  * estimated difference in scaled means: {smd}",
          "\n  * estimated association between the unmeasured confounder and the ",
          "outcome: {outcome_association}\n"
        )
  }
  return(o)
}

#' Adjust an observed coefficient from a loglinear model with a binary confounder
#'
#' @param effect Numeric. Observed exposure - outcome effect from a loglinear
#'    model. This can be the beta coefficient, the lower confidence bound of
#'    the beta coefficient, or the upper confidence bound of the beta
#'    coefficient.
#' @param exposed_p Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the exposed population
#' @param unexposed_p Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the unexposed population
#' @param outcome_association Numeric. Estimated association
#'    between the unmeasured confounder and the outcome.
#' @param verbose Logical. Indicates whether to print informative message.
#'    Default: `TRUE`
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' adjust_coef_with_binary(1.1, 0.5, 0.3, 1.3)
adjust_coef_with_binary <- function(effect, exposed_p, unexposed_p, outcome_association, verbose = TRUE) {
  check_prevalences(unexposed_p, exposed_p)

  confounding_factor <- log(
    (exp(outcome_association) * exposed_p + (1 - exposed_p)) /
      (exp(outcome_association) * unexposed_p + (1 - unexposed_p))
  )

  effect_adj <- effect - confounding_factor
  o <- tibble::tibble(
    effect_adjusted = effect_adj,
    effect_observed = effect,
    exposed_p = exposed_p,
    unexposed_p = unexposed_p,
    outcome_association = outcome_association
  )
  if (verbose) {
    message_glue(
      "The observed effect ({round(effect, 2)}) ",
      "is updated to {round(effect_adj, 2)} ",
      "by an confounder with the following specifications:",
      "\n  * estimated prevalence of the unmeasured confounder ",
      "in the exposed population: {round(exposed_p, 2)}\n  * estimated prevalence of ",
      "the unmeasured confounder in the unexposed population: {round(unexposed_p, 2)}",
      "\n  * estimated association between the unmeasured confounder and the ",
      "outcome: {round(outcome_association, 2)}\n"
    )
  }
  return(o)
}
#' Adjust an observed relative risk for a normally distributed
#' confounder
#'
#' @param effect Numeric positive value. Observed exposure - outcome relative risk.
#'    This can be the point estimate, lower confidence bound, or upper
#'    confidence bound.
#' @param smd Numeric. Estimated difference in scaled means between the
#'    unmeasured confounder in the exposed population and unexposed population
#' @param outcome_association Numeric. Estimated association
#'    between the unmeasured confounder and the outcome.
#' @param verbose Logical. Indicates whether to print informative message.
#'    Default: `TRUE`
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' adjust_rr(1.2, 0.5, 1.1)
adjust_rr <- function(effect, smd, outcome_association, verbose  = TRUE) {
  rr <- effect
  check_gamma(outcome_association)
  check_effect(rr)
  rr_adj <- rr / (outcome_association^smd)

  o <- tibble::tibble(
    rr_adjusted = rr_adj,
    rr_observed = rr,
    smd = smd,
    outcome_association = outcome_association
  )
  if (verbose) {
    message_glue(
      "The observed effect (RR: {round(rr, 2)}) ",
      "is updated to RR: {round(rr_adj, 2)} ",
      "by a confounder with the following specifications:",
      "\n  * estimated difference in scaled means: {smd}",
      "\n  * estimated association (RR) between the unmeasured confounder and the ",
      "outcome: {round(outcome_association, 2)}\n"
    )
  }
  return(o)
}

#' Adjust an observed hazard ratio for a normally distributed
#' confounder
#'
#' @param effect Numeric positive value. Observed exposure - outcome hazard ratio.
#'    This can be the point estimate, lower confidence bound, or upper
#'    confidence bound.
#' @param smd Numeric. Estimated difference in scaled means between the
#'    unmeasured confounder in the exposed population and unexposed population
#' @param outcome_association Numeric. Estimated association
#'    between the unmeasured confounder and the outcome.
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
#' adjust_hr(0.9, -0.9, 1.3)
adjust_hr <- function(effect, smd, outcome_association, verbose = TRUE, hr_correction = FALSE) {
  hr <- effect
  if (hr_correction) {
    hr <- hr_transform(hr)
    outcome_association <- hr_transform(outcome_association)
  }
  o <- adjust_rr(hr, smd, outcome_association, verbose = FALSE)

  output_type <- ifelse(hr_correction, 'RR', 'HR')


  if (verbose) {
    message_glue(
      "The observed effect ({output_type}: {round(o$rr_observed, 2)}) ",
      "is updated to {output_type}: {round(o$rr_adjusted, 2)} ",
      "by a confounder with the following specifications:",
      "\n  * estimated difference in scaled means: {smd}",
      "\n  * estimated association ({output_type}) between the unmeasured confounder and the ",
      "outcome: {round(outcome_association, 2)}\n",
      "{ifelse(hr_correction, 'You opted to use the hazard ratio correction to convert your hazard ratios to approximate risk ratios.\nThis is a good idea if the outcome is common (>15%).',
      '')}",

    )
  }

  if (!hr_correction) {
    names(o)[1] <- "hr_adjusted"
    names(o)[2] <- "hr_observed"
  }

  return(o)
}
#' Adjust an observed odds ratio for a normally distributed
#' confounder
#'
#' @param effect Numeric positive value. Observed exposure - outcome odds ratio.
#'    This can be the point estimate, lower confidence bound, or upper
#'    confidence bound.
#' @param smd Numeric. Estimated difference in scaled means between the
#'    unmeasured confounder in the exposed population and unexposed population
#' @param outcome_association Numeric. Estimated association
#'    between the unmeasured confounder and the outcome.
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
#' adjust_or(1.2, 0.9, 1.3)
adjust_or <- function(effect, smd, outcome_association, verbose = TRUE, or_correction = FALSE) {
  or <- effect
  if (or_correction) {
    or <- or_transform(or)
    outcome_association <- or_transform(outcome_association)
  }
  o <- adjust_rr(or, smd, outcome_association, verbose = FALSE)

  output_type <- ifelse(or_correction, 'RR', 'OR')

  if (verbose) {
    message_glue(
      "The observed effect ({output_type}: {round(o$rr_observed, 2)}) ",
      "is updated to {output_type}: {round(o$rr_adjusted, 2)} ",
      "by a confounder with the following specifications:",
      "\n  * estimated difference in scaled means: {smd}",
      "\n  * estimated association ({output_type}) between the unmeasured confounder and the ",
      "outcome: {round(outcome_association, 2)}\n",
      "{ifelse(or_correction, 'You opted to use the odds ratio correction to convert your odds ratios to approximate risk ratios.\nThis is a good idea if the outcome is common (>15%).',
      '')}"
    )
  }

  if (!or_correction) {
    names(o)[1] <- "or_adjusted"
    names(o)[2] <- "or_observed"
  }

  return(o)
}

#' Adjust an observed relative risk with a binary confounder
#'
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
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' adjust_rr_with_binary(1.1, 0.5, 0.3, 1.3)
adjust_rr_with_binary <- function(effect, exposed_p, unexposed_p, outcome_association, verbose = TRUE) {
  rr <- effect
  check_prevalences(unexposed_p, exposed_p)

  confounding_factor <- (outcome_association * exposed_p + (1 - exposed_p)) /
    ((outcome_association * unexposed_p) + (1 - unexposed_p))

  rr_adj <- rr / confounding_factor
  o <- tibble::tibble(
    rr_adjusted = rr_adj,
    rr_observed = rr,
    exposed_p = exposed_p,
    unexposed_p = unexposed_p,
    outcome_association = outcome_association
  )
  if (verbose) {
    message_glue(
      "The observed effect ({round(rr, 2)}) ",
      "is updated to {round(rr_adj, 2)} ",
      "by an confounder with the following specifications:",
      "\n  * estimated prevalence of the unmeasured confounder ",
      "in the exposed population: {round(exposed_p, 2)}\n  * estimated prevalence of ",
      "the unmeasured confounder in the unexposed population: {round(unexposed_p, 2)}",
      "\n  * estimated association between the unmeasured confounder and the ",
      "outcome: {round(outcome_association, 2)}\n"
    )
  }
  return(o)
}
#' Adjust an observed hazard ratio with a binary confounder
#'
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
#' adjust_hr_with_binary(0.8, 0.1, 0.5, 1.8)
adjust_hr_with_binary <- function(effect, exposed_p, unexposed_p, outcome_association, verbose = TRUE, hr_correction = FALSE) {
  hr <- effect
  if (hr_correction) {
    hr <- hr_transform(hr)
    outcome_association <- hr_transform(outcome_association)
  }
  o <- adjust_rr_with_binary(hr, exposed_p, unexposed_p, outcome_association, verbose = FALSE)

  output_type <- ifelse(hr_correction, 'RR', 'HR')

  if (verbose) {
    message_glue(
      "The observed effect ({output_type}: {round(o$rr_observed, 2)}) ",
      "is updated to {output_type}: {round(o$rr_adjusted, 2)} ",
      "by an confounder with the following specifications:",
      "\n  * estimated prevalence of the unmeasured confounder ",
      "in the exposed population: {round(exposed_p, 2)}\n  * estimated prevalence of ",
      "the unmeasured confounder in the unexposed population: {round(unexposed_p, 2)}",
      "\n  * estimated association between the unmeasured confounder and the ",
      "outcome: {round(outcome_association, 2)}\n",
      "{ifelse(hr_correction, 'You opted to use the hazard ratio correction to convert your hazard ratios to approximate risk ratios.\nThis is a good idea if the outcome is common (>15%).',
      '')}"
    )
  }

  if (!hr_correction) {
    names(o)[1] <- "hr_adjusted"
    names(o)[2] <- "hr_observed"
  }
  return(o)
}


#' Adjust an observed odds ratio with a binary confounder
#'
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
#' adjust_or_with_binary(3, 1, 0, 3)
#' adjust_or_with_binary(3, 1, 0, 3, or_correction = TRUE)
adjust_or_with_binary <- function(effect, exposed_p, unexposed_p, outcome_association, verbose = TRUE, or_correction = FALSE) {
  or <- effect
  if (or_correction) {
    or <- or_transform(or)
    outcome_association <- or_transform(outcome_association)
  }
  o <- adjust_rr_with_binary(or, exposed_p, unexposed_p, outcome_association, verbose = FALSE)

  output_type <- ifelse(or_correction, 'RR', 'OR')

  if (verbose) {
    message_glue(
      "The observed effect ({output_type}: {round(o$rr_observed, 2)}) ",
      "is updated to {output_type}: {round(o$rr_adjusted, 2)} ",
      "by an confounder with the following specifications:",
      "\n  * estimated prevalence of the unmeasured confounder ",
      "in the exposed population: {round(exposed_p, 2)}\n  * estimated prevalence of ",
      "the unmeasured confounder in the unexposed population: {round(unexposed_p, 2)}",
      "\n  * estimated association between the unmeasured confounder and the ",
      "outcome: {round(outcome_association, 2)}\n",
      "{ifelse(or_correction, 'You opted to use the odds ratio correction to convert your odds ratios to approximate risk ratios.\nThis is a good idea if the outcome is common (>15%).',
      '')}"
    )
  }

  if (!or_correction) {
    names(o)[1] <- "or_adjusted"
    names(o)[2] <- "or_observed"
  }

  return(o)
}

#' @rdname adjust_coef
#' @export
adjust_coef_with_continuous <- adjust_coef

#' @rdname adjust_rr
#' @export
adjust_rr_with_continuous <- adjust_rr

#' @rdname adjust_or
#' @export
adjust_or_with_continuous <- adjust_or

#' @rdname adjust_hr
#' @export
adjust_hr_with_continuous <- adjust_hr
