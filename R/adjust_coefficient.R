#' Adjust an observed regression coefficient for a normally distributed
#' confounder
#'
#' @param effect_observed Numeric. Observed exposure - outcome effect from a regression
#'    model. This can be the beta coefficient, the lower confidence bound of
#'    the beta coefficient, or the upper confidence bound of the beta
#'    coefficient.
#' @param exposure_confounder_effect Numeric. Estimated difference in scaled means between the
#'    unmeasured confounder in the exposed population and unexposed population
#' @param confounder_outcome_effect Numeric. Estimated relationship
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
adjust_coef <-
  function(effect_observed,
           exposure_confounder_effect,
           confounder_outcome_effect,
           verbose = getOption("tipr.verbose", TRUE)) {
    effect_adj <-
      effect_observed - confounder_outcome_effect * exposure_confounder_effect
    o <- tibble::tibble(
      effect_adjusted = effect_adj,
      effect_observed = effect_observed,
      exposure_confounder_effect = exposure_confounder_effect,
      confounder_outcome_effect = confounder_outcome_effect
    )
    if (verbose) {
      message_cli(c(
        "i" = "The observed effect ({round(effect_observed, 2)}) \\
        is updated to {round(effect_adj, 2)} \\
        by a confounder with the following specifications:",
        "*" = "estimated difference in scaled means: {exposure_confounder_effect}",
        "*" = "estimated relationship between the unmeasured confounder and the \\
        outcome: {confounder_outcome_effect}"
      ))
    }
    return(o)
  }

#' Adjust an observed coefficient from a regression model with a binary
#' confounder
#'
#' @param effect_observed Numeric. Observed exposure - outcome effect from a
#'   loglinear model. This can be the beta coefficient, the lower confidence
#'   bound of the beta coefficient, or the upper confidence bound of the beta
#'   coefficient.
#' @param exposed_confounder_prev Numeric between 0 and 1. Estimated prevalence
#'   of the unmeasured confounder in the exposed population
#' @param unexposed_confounder_prev Numeric between 0 and 1. Estimated
#'   prevalence of the unmeasured confounder in the unexposed population
#' @param confounder_outcome_effect Numeric. Estimated relationship between the
#'   unmeasured confounder and the outcome.
#' @param loglinear Logical. Calculate the adjusted coefficient from a loglinear
#'   model instead of a linear model (the default). When `loglinear = FALSE`,
#'   `adjust_coef_with_binary()` is equivalent to `adjust_coef()` where
#'   `exposure_confounder_effect` is the difference in prevalences.
#' @param verbose Logical. Indicates whether to print informative message.
#'   Default: `TRUE`
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' adjust_coef_with_binary(1.1, 0.5, 0.3, 1.3)
adjust_coef_with_binary <-
  function(effect_observed,
           exposed_confounder_prev,
           unexposed_confounder_prev,
           confounder_outcome_effect,
           loglinear = FALSE,
           verbose = getOption("tipr.verbose", TRUE)) {
    check_prevalences(unexposed_confounder_prev, exposed_confounder_prev)
    if (loglinear) {
      confounding_factor <- log((exp(confounder_outcome_effect) * exposed_confounder_prev + (1 - exposed_confounder_prev)) /
                                  (
                                    exp(confounder_outcome_effect) * unexposed_confounder_prev + (1 - unexposed_confounder_prev)
                                  ))
      effect_adj <- effect_observed - confounding_factor
      o <- tibble::tibble(
        effect_adjusted = effect_adj,
        effect_observed = effect_observed,
        exposed_confounder_prev = exposed_confounder_prev,
        unexposed_confounder_prev = unexposed_confounder_prev,
        confounder_outcome_effect = confounder_outcome_effect
      )
    } else {
      o <- adjust_coef(
        effect_observed = effect_observed,
        exposure_confounder_effect = exposed_confounder_prev - unexposed_confounder_prev,
        confounder_outcome_effect = confounder_outcome_effect,
        verbose = FALSE
      )
    }


    if (verbose) {
      message_cli(c(
        "i" = "The observed effect ({round(effect_observed, 2)}) \\
        is updated to {round(o$effect_adjusted, 2)} \\
        by a confounder with the following specifications:",
        "*" = "estimated prevalence of the unmeasured confounder \\
        in the exposed population: {round(exposed_confounder_prev, 2)}",
        "*" = "estimated prevalence of \\
        the unmeasured confounder in the unexposed population: \\
        {round(unexposed_confounder_prev, 2)}",

        "*" = "estimated relationship between the unmeasured confounder and \\
        the outcome: {round(confounder_outcome_effect, 2)}"
      ))
    }
    return(o)
  }
#' Adjust an observed risk ratio for a normally distributed
#' confounder
#'
#' @param effect_observed Numeric positive value. Observed exposure - outcome risk ratio.
#'    This can be the point estimate, lower confidence bound, or upper
#'    confidence bound.
#' @param exposure_confounder_effect Numeric. Estimated difference in scaled means between the
#'    unmeasured confounder in the exposed population and unexposed population
#' @param confounder_outcome_effect Numeric. Estimated relationship
#'    between the unmeasured confounder and the outcome.
#' @param verbose Logical. Indicates whether to print informative message.
#'    Default: `TRUE`
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' adjust_rr(1.2, 0.5, 1.1)
adjust_rr <-
  function(effect_observed,
           exposure_confounder_effect,
           confounder_outcome_effect,
           verbose  = TRUE) {
    rr <- effect_observed
    check_gamma(confounder_outcome_effect)
    check_effect(rr)
    rr_adj <-
      rr / (confounder_outcome_effect ^ exposure_confounder_effect)

    o <- tibble::tibble(
      rr_adjusted = rr_adj,
      rr_observed = rr,
      exposure_confounder_effect = exposure_confounder_effect,
      confounder_outcome_effect = confounder_outcome_effect
    )
    if (verbose) {
      message_cli(c(
        "i" = "The observed effect (RR: {round(rr, 2)}) \\
        is updated to RR: {round(rr_adj, 2)} \\
        by a confounder with the following specifications:",
        "*" = "estimated difference in scaled means: {exposure_confounder_effect}",
        "*" = "estimated relationship (RR) between the unmeasured confounder \\
        and the outcome: {round(confounder_outcome_effect, 2)}"
      ))
    }
    return(o)
  }

#' Adjust an observed hazard ratio for a normally distributed
#' confounder
#'
#' @param effect_observed Numeric positive value. Observed exposure - outcome hazard ratio.
#'    This can be the point estimate, lower confidence bound, or upper
#'    confidence bound.
#' @param exposure_confounder_effect Numeric. Estimated difference in scaled means between the
#'    unmeasured confounder in the exposed population and unexposed population
#' @param confounder_outcome_effect Numeric. Estimated relationship
#'    between the unmeasured confounder and the outcome.
#' @param verbose Logical. Indicates whether to print informative message.
#'    Default: `TRUE`
#' @param hr_correction Logical. Indicates whether to use a correction factor.
#'    The methods used for this function are based on risk ratios. For rare
#'    outcomes, a hazard ratio approximates a risk ratio. For common outcomes,
#'    a correction factor is needed. If you have a common outcome (>15%),
#'    set this to `TRUE`. Default: `FALSE`.
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' adjust_hr(0.9, -0.9, 1.3)
adjust_hr <-
  function(effect_observed,
           exposure_confounder_effect,
           confounder_outcome_effect,
           verbose = getOption("tipr.verbose", TRUE),
           hr_correction = FALSE) {
    hr <- effect_observed
    if (hr_correction) {
      hr <- hr_transform(hr)
      confounder_outcome_effect <-
        hr_transform(confounder_outcome_effect)
    }
    o <-
      adjust_rr(hr,
                exposure_confounder_effect,
                confounder_outcome_effect,
                verbose = FALSE)

    output_type <- ifelse(hr_correction, 'RR', 'HR')


    if (verbose) {
      message_cli(c(
        "i" = "The observed effect ({output_type}: {round(o$rr_observed, 2)}) \\
        is updated to {output_type}: {round(o$rr_adjusted, 2)} \\
        by a confounder with the following specifications:",
        "*" = "estimated difference in scaled means: {exposure_confounder_effect}",
        "*" = "estimated relationship ({output_type}) between the unmeasured \\
        confounder and the outcome: {round(confounder_outcome_effect, 2)}"
      ))

      if (hr_correction) message_cli(c(
        "i" = "You opted to use the hazard ratio correction to convert your \\
        hazard ratios to approximate risk ratios. \\
        This is a good idea if the outcome is common (>15%)"
      ))
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
#' @param effect_observed Numeric positive value. Observed exposure - outcome odds ratio.
#'    This can be the point estimate, lower confidence bound, or upper
#'    confidence bound.
#' @param exposure_confounder_effect Numeric. Estimated difference in scaled means between the
#'    unmeasured confounder in the exposed population and unexposed population
#' @param confounder_outcome_effect Numeric. Estimated relationship
#'    between the unmeasured confounder and the outcome.
#' @param verbose Logical. Indicates whether to print informative message.
#'    Default: `TRUE`
#' @param or_correction Logical. Indicates whether to use a correction factor.
#'    The methods used for this function are based on risk ratios. For rare
#'    outcomes, an odds ratio approximates a risk ratio. For common outcomes,
#'    a correction factor is needed. If you have a common outcome (>15%),
#'    set this to `TRUE`. Default: `FALSE`.
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' adjust_or(1.2, 0.9, 1.3)
adjust_or <-
  function(effect_observed,
           exposure_confounder_effect,
           confounder_outcome_effect,
           verbose = getOption("tipr.verbose", TRUE),
           or_correction = FALSE) {
    or <- effect_observed
    if (or_correction) {
      or <- or_transform(or)
      confounder_outcome_effect <-
        or_transform(confounder_outcome_effect)
    }
    o <-
      adjust_rr(or,
                exposure_confounder_effect,
                confounder_outcome_effect,
                verbose = FALSE)

    output_type <- ifelse(or_correction, 'RR', 'OR')

    if (verbose) {
      message_cli(c(
        "i" = "The observed effect ({output_type}: {round(o$rr_observed, 2)}) \\
        is updated to {output_type}: {round(o$rr_adjusted, 2)} \\
        by a confounder with the following specifications:",
        "*" = "estimated difference in scaled means: {exposure_confounder_effect}",
        "*" = "estimated relationship ({output_type}) between the unmeasured \\
        confounder and the  outcome: {round(confounder_outcome_effect, 2)}"
      ))


      if (or_correction) message_cli(c(
        "i" = "You opted to use the odds ratio correction to convert your \\
        odds ratio to approximate risk ratios. \\
        This is a good idea if the outcome is common (>15%)"
      ))
    }

    if (!or_correction) {
      names(o)[1] <- "or_adjusted"
      names(o)[2] <- "or_observed"
    }

    return(o)
  }

#' Adjust an observed risk ratio with a binary confounder
#'
#' @param effect_observed Numeric positive value. Observed exposure - outcome risk ratio.
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
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' adjust_rr_with_binary(1.1, 0.5, 0.3, 1.3)
adjust_rr_with_binary <-
  function(effect_observed,
           exposed_confounder_prev,
           unexposed_confounder_prev,
           confounder_outcome_effect,
           verbose = getOption("tipr.verbose", TRUE)) {
    rr <- effect_observed
    check_prevalences(unexposed_confounder_prev, exposed_confounder_prev)

    confounding_factor <-
      (confounder_outcome_effect * exposed_confounder_prev + (1 - exposed_confounder_prev)) /
      ((confounder_outcome_effect * unexposed_confounder_prev) + (1 - unexposed_confounder_prev))

    rr_adj <- rr / confounding_factor
    o <- tibble::tibble(
      rr_adjusted = rr_adj,
      rr_observed = rr,
      exposed_confounder_prev = exposed_confounder_prev,
      unexposed_confounder_prev = unexposed_confounder_prev,
      confounder_outcome_effect = confounder_outcome_effect
    )
    if (verbose) {
      message_cli(c(
        "The observed effect ({round(rr, 2)}) \\
        is updated to {round(rr_adj, 2)} \\
        by a confounder with the following specifications:",
        "*" = "estimated prevalence of the unmeasured confounder \\
        in the exposed population: {round(exposed_confounder_prev, 2)}",
        "*" = "estimated prevalence of the unmeasured confounder in the \\
        unexposed population: {round(unexposed_confounder_prev, 2)}",
        "*" = "estimated relationship between the unmeasured confounder and \\
        the outcome: {round(confounder_outcome_effect, 2)}"
      ))
    }
    return(o)
  }
#' Adjust an observed hazard ratio with a binary confounder
#'
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
#'    The methods used for this function are based on risk ratios. For rare
#'    outcomes, a hazard ratio approximates a risk ratio. For common outcomes,
#'    a correction factor is needed. If you have a common outcome (>15%),
#'    set this to `TRUE`. Default: `FALSE`.
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' adjust_hr_with_binary(0.8, 0.1, 0.5, 1.8)
adjust_hr_with_binary <-
  function(effect_observed,
           exposed_confounder_prev,
           unexposed_confounder_prev,
           confounder_outcome_effect,
           verbose = getOption("tipr.verbose", TRUE),
           hr_correction = FALSE) {
    hr <- effect_observed
    if (hr_correction) {
      hr <- hr_transform(hr)
      confounder_outcome_effect <-
        hr_transform(confounder_outcome_effect)
    }
    o <-
      adjust_rr_with_binary(hr,
                            exposed_confounder_prev,
                            unexposed_confounder_prev,
                            confounder_outcome_effect,
                            verbose = FALSE)

    output_type <- ifelse(hr_correction, 'RR', 'HR')

    if (verbose) {
      message_cli(c(
        "i" = "The observed effect ({output_type}: {round(o$rr_observed, 2)}) \\
        is updated to {output_type}: {round(o$rr_adjusted, 2)} \\
        by a confounder with the following specifications:",
        "*" = "estimated prevalence of the unmeasured confounder \\
        in the exposed population: {round(exposed_confounder_prev, 2)}",
        "*" = "estimated prevalence of the unmeasured confounder in the \\
        unexposed population: {round(unexposed_confounder_prev, 2)}",
        "*" = "estimated relationship between the unmeasured confounder \\
        and the outcome: {round(confounder_outcome_effect, 2)}"
      ))

      if (hr_correction) message_cli(c(
        "i" = "You opted to use the hazard ratio correction to convert your \\
        hazard ratios to approximate risk ratios. \\
        This is a good idea if the outcome is common (>15%)"
      ))
    }

    if (!hr_correction) {
      names(o)[1] <- "hr_adjusted"
      names(o)[2] <- "hr_observed"
    }
    return(o)
  }


#' Adjust an observed odds ratio with a binary confounder
#'
#' @param effect_observed Numeric positive value. Observed exposure - outcome
#'    odds ratio. This can be the point estimate, lower confidence bound, or
#'     upper confidence bound.
#' @param exposed_confounder_prev Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the exposed population
#' @param unexposed_confounder_prev Numeric between 0 and 1. Estimated prevalence of the
#'    unmeasured confounder in the unexposed population
#' @param confounder_outcome_effect Numeric positive value. Estimated relationship
#'    between the unmeasured confounder and the outcome
#' @param verbose Logical. Indicates whether to print informative message.
#'    Default: `TRUE`
#' @param or_correction Logical. Indicates whether to use a correction factor.
#'    The methods used for this function are based on risk ratios. For rare
#'    outcomes, an odds ratio approximates a risk ratio. For common outcomes,
#'    a correction factor is needed. If you have a common outcome (>15%),
#'    set this to `TRUE`. Default: `FALSE`.
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' adjust_or_with_binary(3, 1, 0, 3)
#' adjust_or_with_binary(3, 1, 0, 3, or_correction = TRUE)
adjust_or_with_binary <-
  function(effect_observed,
           exposed_confounder_prev,
           unexposed_confounder_prev,
           confounder_outcome_effect,
           verbose = getOption("tipr.verbose", TRUE),
           or_correction = FALSE) {
    or <- effect_observed
    if (or_correction) {
      or <- or_transform(or)
      confounder_outcome_effect <-
        or_transform(confounder_outcome_effect)
    }
    o <-
      adjust_rr_with_binary(or,
                            exposed_confounder_prev,
                            unexposed_confounder_prev,
                            confounder_outcome_effect,
                            verbose = FALSE)

    output_type <- ifelse(or_correction, 'RR', 'OR')

    if (verbose) {
      message_cli(c(
        "*" = "The observed effect ({output_type}: {round(o$rr_observed, 2)}) \\
        is updated to {output_type}: {round(o$rr_adjusted, 2)} \\
        by a confounder with the following specifications:",
        "*" = "estimated prevalence of the unmeasured confounder \\
        in the exposed population: {round(exposed_confounder_prev, 2)}",
        "*" = "estimated prevalence of the unmeasured confounder in the \\
        unexposed population: {round(unexposed_confounder_prev, 2)}",
        "*" = "estimated relationship between the unmeasured confounder and \\
        the outcome: {round(confounder_outcome_effect, 2)}"
      ))

      if (or_correction) message_cli(c(
        "i" = "You opted to use the odds ratio correction to convert your \\
        odds ratios to approximate risk ratios. \\
        This is a good idea if the outcome is common (>15%)"
      ))
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
