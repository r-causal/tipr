get_limiting_bound <- function(lb = NULL, ub = NULL) {
  if (is.null(lb) || is.null(ub)) {
    stop("Please input a dataset `d` that contains your observed confidence interval. Be sure your column names match `lb_name` and `ub_name`",
      call. = FALSE
    )
  }
  if (lb < 0 || ub < 0) {
    stop_glue(
      "You input: ({lb}, {ub})\n",
      "We are expecting an odds ratio, hazard ratio, or relative risk,\n",
      "therefore the bounds should not be less than 0."
    )
  }
  if (lb > 1 && ub > 1) {
    return(lb)
  }
  if (lb < 1 && ub < 1) {
    return(ub)
  }
  stop_glue(
    "You input: ({lb}, {ub})\n",
    "Please input a significant result."
  )
}

get_lm_limiting_bound <- function(lb = NULL, ub = NULL) {
  if (is.null(lb) || is.null(ub)) {
    stop("Please input a dataset `d` that contains your observed confidence interval. Be sure your column names match `lb_name` and `ub_name`",
         call. = FALSE
    )
  }

  if (lb > 0 && ub > 0) {
    return(lb)
  }
  if (lb < 0 && ub < 0) {
    return(ub)
  }
  stop_glue(
    "You input: ({lb}, {ub})\n",
    "Please input a significant result."
  )
}

get_limiting_bound_adj <- function(b = NULL, lb = NULL, ub = NULL) {
  if (is.null(lb) || is.null(ub)) {
    stop("Please input a data frame `d` that contains for your observed confidence interval.",
      call. = FALSE
    )
  }
  if (lb < 0 || ub < 0) {
    stop_glue(
      "You input: ({lb}, {ub})\n",
      "We are expecting an odds ratio, hazard ratio, or relative risk,\n",
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
      "You input:\n * `outcome_association`: {gamma}\n",
      "We are expecting a relative risk, odds ratio, or hazard ratio,\n",
      "therefore `outcome_association` should not be less than 0."
    )
  }
}

check_effect <- function(x) {
  if (x < 0) {
    stop_glue(
      "You input:\n * An observed effect of {x}\n",
      "We are expecting a relative risk, odds ratio, or hazard ratio,\n",
      "therefore your effect should not be less than 0."
    )
  }
}



check_prevalences <- function(p0 = NULL, p1 = NULL) {
  if (is.null(p0)) {
    if (p1 < 0 | p1 > 1) {
      stop_glue(
        "You input:\n * `exposed_p`: {p1}\n",
        "The prevalences entered must be between 0 and 1."
      )
    }
  } else if (is.null(p1)) {
    if (p0 < 0 | p0 > 1) {
      stop_glue(
        "You input:\n * `unexposed_p`: {p0}\n",
        "The prevalences entered must be between 0 and 1."
      )
    }
  } else if (p1 < 0 | p0 < 0 | p1 > 1 | p0 > 1) {
    stop_glue(
      "You input:\n * `unexposed_p`: {p0}\n * `exposed_p`: {p1}\n",
      "The prevalences entered must be between 0 and 1."
    )
  }
}

tip_gamma <- function(p0 = NULL,
                      p1 = NULL,
                      b = NULL) {
  if (is.null(p1) || is.null(p0)) {
    stop("Please input at least 2 of the following:\n * `unexposed_p`\n * `exposed_p`\n * `outcome_association`",
      call. = FALSE
    )
  }

  check_prevalences(p0, p1)

  gamma <- ((1 - p1) + b * (p0 - 1)) / (b * p0 - p1)

  if (gamma < 0) {
    stop_glue(
      "Given these prevalences:\n * `unexposed_p`: {p0}\n * `exposed_p`: {p1}\n",
      "There does not exist an unmeasured confounder that could tip this.\n",
      "Please specifiy a larger prevalence difference.\n",
      "(ie: make `unexposed_p` and `exposed_p` farther apart)."
    )
  }
  as.numeric(gamma)
}

tip_p0 <- function(p1 = NULL,
                   gamma = NULL,
                   b = NULL) {
  if (is.null(p1) || is.null(gamma)) {
    stop("Please input at least 2 of the following:\n * `unexposed_p`\n * `exposed_p`\n * `outcome_association`.",
      call. = FALSE
    )
  }

  check_prevalences(p1 = p1)
  check_gamma(gamma)

  p0 <- (p1 * (gamma - 1) - b + 1) / (b * (gamma - 1))

  if (p0 > 1 | p0 < 0) {
    stop_glue(
      "Given these parameters:\n * `exposed_p`: {p1}\n * `outcome_association`: {gamma}\n",
      "There does not exist an unmeasured confounder that could tip this."
    )
  }
  as.numeric(p0)
}


tip_p1 <- function(p0 = NULL,
                   gamma = NULL,
                   b = NULL) {
  if (is.null(p0) || is.null(gamma)) {
    stop("Please input at least 2 of the following:\n * `unexposed_p`\n * `exposed_p`\n * `outcome_association`.",
      call. = FALSE
    )
  }

  check_prevalences(p0 = p0)
  check_gamma(gamma)

  p1 <- ((b - 1) / (gamma - 1)) + b * p0

  if (p1 > 1 | p1 < 0) {
    stop_glue(
      "Given these parameters:\n * `unexposed_p`: {p0}\n * `outcome_association`: {gamma}\n",
      "There does not exist an unmeasured confounder that could tip this."
    )
  }
  as.numeric(p1)
}

tip_n <- function(p0, p1, gamma, b) {
  check_prevalences(p0, p1)
  check_gamma(gamma)

  n <- -log(b) / (log(gamma * p0 + (1 - p0)) - log(gamma * p1 + (1 - p1)))
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
  (1 - (0.5^sqrt(hr))) / (1 - (0.5^sqrt(1 / hr)))
}

or_transform <- function(or) {
  if (is.null(or)) {
    return(NULL)
  }
  sqrt(or)
}
