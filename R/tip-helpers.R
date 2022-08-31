get_limiting_bound <- function(lb = NULL, ub = NULL) {
  if (is.null(lb) || is.null(ub)) {
    stop(
      "Please input a dataset `d` that contains your observed confidence interval. Be sure your column names match `lb_name` and `ub_name`",
      call. = FALSE
    )
  }
  if (lb < 0 || ub < 0) {
    stop_glue(
      "You input: ({lb}, {ub})\n",
      "We are expecting an odds ratio, hazard ratio, or risk ratio,\n",
      "therefore the bounds should not be less than 0."
    )
  }
  if (lb > 1 && ub > 1) {
    return(lb)
  }
  if (lb < 1 && ub < 1) {
    return(ub)
  }
  stop_glue("You input: ({lb}, {ub})\n",
            "Please input a significant result.")
}

get_lm_limiting_bound <- function(lb = NULL, ub = NULL) {
  if (is.null(lb) || is.null(ub)) {
    stop(
      "Please input a dataset `d` that contains your observed confidence interval. Be sure your column names match `lb_name` and `ub_name`",
      call. = FALSE
    )
  }

  if (lb > 0 && ub > 0) {
    return(lb)
  }
  if (lb < 0 && ub < 0) {
    return(ub)
  }
  stop_glue("You input: ({lb}, {ub})\n",
            "Please input a significant result.")
}

get_limiting_bound_adj <- function(b = NULL,
                                   lb = NULL,
                                   ub = NULL) {
  if (is.null(lb) || is.null(ub)) {
    stop(
      "Please input a data frame `d` that contains for your observed confidence interval.",
      call. = FALSE
    )
  }
  if (lb < 0 || ub < 0) {
    stop_glue(
      "You input: ({lb}, {ub})\n",
      "We are expecting an odds ratio, hazard ratio, or risk ratio,\n",
      "therefore the lower or upper bounds in `d` should not be less than 0."
    )
  }
  if (b > 1) {
    return(lb)
  }
  if (b < 1) {
    return(ub)
  }
}

check_gamma <- function(gamma = NULL) {
  if (!is.null(gamma) && gamma < 0) {
    stop_glue(
      "You input:\n * `outcome_effect`: {gamma}\n",
      "We are expecting a risk ratio, odds ratio, or hazard ratio,\n",
      "therefore `outcome_effect` should not be less than 0."
    )
  }
}

check_effect <- function(x) {
  if (x < 0) {
    stop_glue(
      "You input:\n * An observed effect of {x}\n",
      "We are expecting a risk ratio, odds ratio, or hazard ratio,\n",
      "therefore your effect should not be less than 0."
    )
  }
}



check_prevalences <- function(p0 = NULL, p1 = NULL) {
  if (is.null(p0)) {
    if (any(p1 < 0 | p1 > 1)) {
      stop_glue(
        "You input:\n * `exposed_confounder_prev`: {p1}\n",
        "The prevalences entered must be between 0 and 1."
      )
    }
  } else if (is.null(p1)) {
    if (any(p0 < 0 | p0 > 1)) {
      stop_glue(
        "You input:\n * `unexposed_confounder_prev`: {p0}\n",
        "The prevalences entered must be between 0 and 1."
      )
    }
  } else if (any(p1 < 0 | p0 < 0 | p1 > 1 | p0 > 1)) {
    stop_glue(
      "You input:\n * `unexposed_confounder_prev`: {p0}\n * `exposed_confounder_prev`: {p1}\n",
      "The prevalences entered must be between 0 and 1."
    )
  }
}

tip_gamma <- function(p0 = NULL,
                      p1 = NULL,
                      b = NULL) {
  if (is.null(p1) || is.null(p0)) {
    stop(
      "Please input at least 2 of the following:\n * `unexposed_confounder_prev`\n * `exposed_confounder_prev`\n * `outcome_effect`",
      call. = FALSE
    )
  }

  check_prevalences(p0, p1)

  gamma <- ((1 - p1) + b * (p0 - 1)) / (b * p0 - p1)

  if (gamma < 0) {
    stop_glue(
      "Given these prevalences:\n * `unexposed_confounder_prev`: {p0}\n * `exposed_confounder_prev`: {p1}\n",
      "There does not exist an unmeasured confounder that could tip this.\n",
      "Please specifiy a larger prevalence difference.\n",
      "(ie: make `unexposed_confounder_prev` and `exposed_confounder_prev` farther apart)."
    )
  }
  as.numeric(gamma)
}

check_r2 <- function(r2, exposure = FALSE, effect, se, df) {
  if (any(r2 < 0) | any(r2 > 1)) {
    stop_glue("You input:\n {r2}\n",
              "The partial R2 values entered must be between 0 and 1.")
  }
  if (exposure) {
    if (any(r2 == 1)) {
      stop_glue(
        "You input:\n * `exposure_r2`: {r2}\n",
        "This means 100% of the residual variation in the exposure ",
        "is explained by the unmeasured confounder, meaning regardless ",
        "of the unmeasured confounder - outcome relationship, this ",
        "will be \"tipped\"."
      )
    }
    limit <- sensemakr::partial_r2(effect / se, df)
    if (any(r2 < limit)) {
      stop_glue(
        "You input:\n * `exposure_r2`: {r2[r2 < limit]}\n",
        "It is not possible to tip this result with any unmeasured ",
        "confounder - outcome relationship. In fact, if your ",
        "unmeasured confounder explained 100% of the residual ",
        "variation in your outcome, the partial R2 for the unmeasured ",
        "confounder - exposure relationship would have to be ",
        "{round(limit, 3)} for the exposure - outcome relationship ",
        "to be explained away."
      )
    }
  }
}
tip_exposure_r2 <- function(effect, se, df, outcome_r2) {
  if (is.null(outcome_r2)) {
    stop(
      "Please input at least one of the following:\n * `exposure_r2`\n * `outcome_r2`",
      call. = FALSE
    )
  }
  check_r2(outcome_r2)

  exposure_r2 <-
    effect ^ 2 / (effect ^ 2 + se ^ 2 * df * outcome_r2)
  if (any(exposure_r2 > 1)) {
    stop_glue(
      "Given the input:\n * `effect`: {effect}\n * `outcome_r2`: {outcome_r2[exposure_r2 > 1]}\n",
      "There does not exist an unmeasured confounder that could tip this.\n",
    )
  }
  as.numeric(exposure_r2)
}
tip_exposure_r2_bound <-
  function(effect, se, df, outcome_r2, alpha) {
    if (is.null(outcome_r2)) {
      stop(
        "Please input at least one of the following:\n * `exposure_r2`\n * `outcome_r2`",
        call. = FALSE
      )
    }
    check_r2(outcome_r2)

    t_star <- stats::qt(alpha / 2, df = df, lower.tail = F)
    lb <- effect - t_star * se
    ub <- effect + t_star * se

    y <- outcome_r2
    a <- effect
    b <- se
    c <- df
    d <- t_star
    exposure_r2 <-
      (
        2 * a ^ 4 - (2 * a ^ 2 * b ^ 2 * d ^ 2 * y) / (1 - c) + (2 * a ^ 2 * b ^
                                                                   2 * d ^ 2) / (1 - c) +
          2 * a ^ 2 * b ^ 2 * c * y + 2 * a ^ 2 * b ^ 2 * d ^
          2 * y - 2 * a ^ 2 * b ^ 2 * d ^ 2 -
          sqrt((
            -2 * a ^ 4 + (2 * a ^ 2 * b ^ 2 * d ^ 2 * y) / (1 - c) -
              (2 * a ^ 2 * b ^ 2 * d ^ 2) / (1 - c) -
              2 * a ^ 2 * b ^ 2 * c * y - 2 * a ^ 2 * b ^
              2 * d ^ 2 * y +
              2 * a ^ 2 * b ^ 2 * d ^ 2 + 2 * b ^ 4 * c * d ^
              2 * y ^ 2 -
              (2 * b ^ 4 * d ^ 2 * y ^ 2) / (1 - c) - 2 * b ^
              4 * c * d ^ 2 * y +
              (2 * b ^ 4 * d ^ 2 * y) / (1 - c) + 2 * b ^
              4 * d ^ 2 * y ^ 2 -
              2 * b ^ 4 * d ^ 2 * y
          ) ^ 2 - 4 * (a ^ 4 + 2 * a ^ 2 * b ^ 2 * c * y +
                         b ^ 4 * c ^
                         2 * y ^ 2) *
            (
              a ^ 4 - (2 * a ^ 2 * b ^ 2 * d ^ 2 * y) / (1 - c) +
                (2 * a ^ 2 * b ^ 2 * d ^ 2) / (1 - c) +
                2 * a ^ 2 * b ^ 2 * d ^ 2 * y - 2 * a ^
                2 * b ^ 2 * d ^ 2 -
                (2 * b ^ 4 * d ^ 4 * y ^ 2) / (1 - c) +
                (b ^ 4 * d ^ 4 * y ^ 2) / (1 - c) ^ 2 +
                (4 * b ^ 4 * d ^ 4 * y) / (1 - c) -
                (2 * b ^ 4 * d ^ 4 * y) / (1 - c) ^ 2 -
                (2 * b ^ 4 * d ^ 4) / (1 - c) + (b ^ 4 * d ^
                                                   4) / (1 - c) ^ 2 +
                b ^ 4 * d ^ 4 * y ^ 2 - 2 * b ^ 4 * d ^
                4 * y + b ^ 4 *  d ^ 4
            )
          ) -
          2 * b ^ 4 * c * d ^ 2 * y ^ 2 + (2 * b ^ 4 * d ^
                                             2 * y ^ 2) / (1 - c) +
          2 * b ^ 4 * c * d ^ 2 * y - (2 * b ^ 4 * d ^ 2 * y) /
          (1 - c) -
          2 * b ^ 4 * d ^ 2 * y ^ 2 + 2 * b ^ 4 * d ^ 2 * y
      ) /
      (2 * (a ^ 4 + 2 * a ^ 2 * b ^ 2 * c * y + b ^ 4 * c ^ 2 * y ^ 2))
    if (exposure_r2 > 1) {
      stop_glue(
        "Given the inputs:\n * `effect`: {effect}\n * `se`: {se}\n * `df`: {df}\n",
        "The observed confidence bounds would be {lb}, {ub}. Given the inputs:",
        "\n * observed bounds: {lb}, {ub} \n * outcome_r2`: {outcome_r2}\n",
        "There does not exist an unmeasured confounder that could tip",
        "the bound.\n",
      )
    }
    as.numeric(exposure_r2)
  }



tip_outcome_r2 <- function(effect, se, df, exposure_r2) {
  if (is.null(exposure_r2)) {
    stop(
      "Please input at least one of the following:\n * `exposure_r2`\n * `outcome_r2`",
      call. = FALSE
    )
  }
  check_r2(
    exposure_r2,
    exposure = TRUE,
    effect = effect,
    se = se,
    df = df
  )

  outcome_r2 <-
    (effect ^ 2 - effect ^ 2 * exposure_r2) / (se ^ 2 * df * exposure_r2)
  if (any(outcome_r2 > 1)) {
    stop_glue(
      "Given the input:\n * `effect`: {effect}\n * `exposure_r2`: {exposure_r2[outcome_r2 > 1]}\n",
      "There does not exist an unmeasured confounder that could tip this.\n",
    )
  }
  as.numeric(outcome_r2)
}

tip_outcome_r2_bound <-
  function(effect, se, df, exposure_r2, alpha) {
    if (is.null(exposure_r2)) {
      stop(
        "Please input at least one of the following:\n * `exposure_r2`\n * `outcome_r2`",
        call. = FALSE
      )
    }
    check_r2(
      exposure_r2,
      exposure = TRUE,
      effect = effect,
      se = se,
      df = df
    )

    t_star <- stats::qt(alpha / 2, df = df, lower.tail = F)
    lb <- effect - t_star * se
    ub <- effect + t_star * se

    y <- exposure_r2
    a <- effect
    b <- se
    c <- df
    d <- t_star

    outcome_r2 <-
      (
        b ^ 2 * (-c) * (
          2 * a ^ 2 * c ^ 2 * y ^ 2 - 2 * a ^ 2 * c ^ 2 * y - 2 * a ^ 2 *
            c * d ^ 2 * y + 2 * a ^ 2 * c * d ^ 2 - 4 * a ^ 2 * c * y ^ 2 +
            4 * a ^ 2 * c * y + 2 * a ^ 2 * d ^ 2 * y - 2 * a ^
            2 * d ^ 2 +
            2 * a ^ 2 * y ^ 2 - 2 * a ^ 2 * y - 2 * b ^ 2 * c ^ 2 * d ^ 2 * y -
            2 * b ^ 2 * c * d ^ 4 + 2 * b ^ 2 * c * d ^ 2 * y
        ) -
          sqrt(
            b ^ 4 * c ^ 2 * (
              2 * a ^ 2 * c ^ 2 * y ^ 2 - 2 * a ^ 2 * c ^ 2 * y -
                2 * a ^ 2 * c * d ^ 2 * y + 2 * a ^
                2 * c * d ^ 2 -
                4 * a ^ 2 * c * y ^ 2 + 4 * a ^ 2 * c * y + 2 * a ^ 2 * d ^ 2 * y -
                2 * a ^ 2 * d ^ 2 + 2 * a ^ 2 * y ^ 2 -
                2 * a ^ 2 * y -
                2 * b ^ 2 * c ^ 2 * d ^ 2 * y - 2 * b ^ 2 * c * d ^ 4 +
                2 * b ^ 2 * c * d ^ 2 * y
            ) ^ 2 - 4 * b ^ 4 * c ^ 2 *
              (c ^ 2 * y ^ 2 + 2 * c * d ^ 2 * y - 2 * c * y ^ 2 +
                 d ^ 4 - 2 * d ^ 2 * y + y ^ 2) *
              (
                a ^ 4 * c ^ 2 * y ^ 2 - 2 * a ^ 4 * c ^ 2 * y + a ^ 4 * c ^ 2 -
                  2 * a ^ 4 * c * y ^ 2 +
                  4 * a ^ 4 * c * y - 2 * a ^ 4 * c + a ^ 4 * y ^ 2 -
                  2 * a ^ 4 * y +
                  a ^ 4 + 2 * a ^ 2 * b ^ 2 * c ^ 2 * d ^ 2 * y -
                  2 * a ^  2 * b ^ 2 * c ^ 2 * d ^ 2 -
                  2 * a ^ 2 * b ^ 2 * c * d ^ 2 * y +
                  2 * a ^ 2 * b ^ 2 * c * d ^ 2 +
                  b ^ 4 * c ^ 2 * d ^ 4
              )
          )
      ) / (2 * b ^ 4 * c ^ 2 * (c ^ 2 * y ^ 2 + 2 * c * d ^ 2 * y -
                                  2 * c * y ^
                                  2 + d ^ 4 - 2 * d ^ 2 * y + y ^ 2))
    if (outcome_r2 > 1) {
      stop_glue(
        "Given the inputs:\n * `effect`: {effect}\n * `se`: {se}\n * `df`: {df}\n",
        "The observed confidence bounds would be {lb}, {ub}. Given the inputs:",
        "\n * observed bounds: {lb}, {ub} \n * exposure_r2`: {exposure_r2}\n",
        "There does not exist an unmeasured confounder that could tip",
        "the bound.\n",
      )
    }
    as.numeric(outcome_r2)
  }



tip_p0 <- function(p1 = NULL,
                   gamma = NULL,
                   b = NULL) {
  if (is.null(p1) || is.null(gamma)) {
    stop(
      "Please input at least 2 of the following:\n * `unexposed_confounder_prev`\n * `exposed_confounder_prev`\n * `outcome_effect`.",
      call. = FALSE
    )
  }

  check_prevalences(p1 = p1)
  check_gamma(gamma)

  p0 <- (p1 * (gamma - 1) - b + 1) / (b * (gamma - 1))

  if (p0 > 1 | p0 < 0) {
    stop_glue(
      "Given these parameters:\n * `exposed_confounder_prev`: {p1}\n * `outcome_effect`: {gamma}\n",
      "There does not exist an unmeasured confounder that could tip this."
    )
  }
  as.numeric(p0)
}


tip_p1 <- function(p0 = NULL,
                   gamma = NULL,
                   b = NULL) {
  if (is.null(p0) || is.null(gamma)) {
    stop(
      "Please input at least 2 of the following:\n * `unexposed_confounder_prev`\n * `exposed_confounder_prev`\n * `outcome_effect`.",
      call. = FALSE
    )
  }

  check_prevalences(p0 = p0)
  check_gamma(gamma)

  p1 <- ((b - 1) / (gamma - 1)) + b * p0

  if (p1 > 1 | p1 < 0) {
    stop_glue(
      "Given these parameters:\n * `unexposed_confounder_prev`: {p0}\n * `outcome_effect`: {gamma}\n",
      "There does not exist an unmeasured confounder that could tip this."
    )
  }
  as.numeric(p1)
}

tip_n <- function(p0, p1, gamma, b) {
  check_prevalences(p0, p1)
  check_gamma(gamma)

  n <-
    -log(b) / (log(gamma * p0 + (1 - p0)) - log(gamma * p1 + (1 - p1)))
  if (n < 0) {
    n <- 0
    warning_glue("The observed effect {b} would not tip with the unmeasured confounder given.")
  } else if (n < 1) {
    warning_glue("The observed effect {b} would tip with < 1 of the given unmeasured confounders.")
  }

  as.numeric(n)
}

# e_value <- function(lb, ub) {
#   observed_covariate_e_value(lb, ub, 1, 1)
# }

hr_transform <- function(hr) {
  if (is.null(hr)) {
    return(NULL)
  }
  (1 - (0.5 ^ sqrt(hr))) / (1 - (0.5 ^ sqrt(1 / hr)))
}

or_transform <- function(or) {
  if (is.null(or)) {
    return(NULL)
  }
  sqrt(or)
}
