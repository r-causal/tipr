#' Tip a linear model result with a continuous confounder.
#'
#' choose one of the following, and the other will be estimated:
#' * `smd`
#' * `outcome_association`
#'
#' @param effect Numeric. Observed exposure - outcome effect from a regression
#'    model. This can be the beta coefficient, the lower confidence bound of
#'    the beta coefficient, or the upper confidence bound of the beta
#'    coefficient.
#' @param smd Numeric. Estimated scaled mean difference between the
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
#' @return Data frame.
#'
#' @examples
#' ## to estimate the association between an unmeasured confounder and outcome
#' ## needed to tip analysis
#' tip_lm(1.2, smd = -2)
#'
#' ## to estimate the number of unmeasured confounders specified needed to tip
#' ## the analysis
#' tip_lm(1.2, smd = -2, outcome_association = -0.05)
#'
#' ## Example with broom
#' if (requireNamespace("broom", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE)) {
#'   lm(wt ~ mpg, data = mtcars) %>%
#'    broom::tidy(conf.int = TRUE) %>%
#'    dplyr::filter(term == "mpg") %>%
#'    dplyr::pull(conf.low) %>%
#'    tip_lm(outcome_association = 2.5)
#'}
#' @export
tip_lm <- function(effect, smd = NULL, outcome_association = NULL, verbose = TRUE, correction_factor = "none") {

  o <- purrr::map(
    effect,
    ~ tip_lm_one(.x,
              smd = smd,
              outcome_association = outcome_association,
              verbose = verbose,
              correction_factor = correction_factor
    )
  )
  do.call(rbind, o)
}

tip_lm_one <- function(b, smd, outcome_association, verbose, correction_factor) {

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
    outcome_association <- b / smd
  } else if (is.null(smd)) {
    smd <- b / outcome_association
  } else {
    n_unmeasured_confounders <- b / (smd * outcome_association)
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

#' @rdname tip_lm
#' @export
lm_tip <- function(effect, smd, outcome_association, verbose = TRUE) {
  .Deprecated("tip_lm")
  tip_lm(effect, smd, outcome_association, verbose)
}
