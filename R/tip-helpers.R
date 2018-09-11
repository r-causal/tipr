get_limiting_bound <- function(lb = NULL, ub = NULL) {
  if (is.null(lb) || is.null(ub)) {
    stop("Please input values for your observed confidence interval (`lb`, `ub`).",
         call. = FALSE)
  }
  if (lb < 0 || ub < 0) {
    stop_glue("You input: ({lb}, {ub})\n",
              "We are expecting an odds ratio, hazard ratio, or relative risk,\n",
              "therefore `lb` or `ub` should not be less than 0.")
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

get_limiting_bound_adj <- function(b = NULL, lb = NULL, ub = NULL) {
  if (is.null(lb) || is.null(ub)) {
    stop("Please input values for your observed confidence interval (`lb_adj`, `ub_adj`).",
         call. = FALSE)
  }
  if (lb < 0 || ub < 0) {
    stop_glue("You input: ({lb}, {ub})\n",
              "We are expecting an odds ratio, hazard ratio, or relative risk,\n",
              "therefore `lb_adj` or `ub_adj` should not be less than 0.")
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
    stop_glue("You input:\n * `gamma`: {gamma}\n",
              "We are expecting an odds ratio, hazard ratio, or relative risk,\n",
              "therefore `gamma` should not be less than 0.")
  }
}

check_prevalences <- function(p0 = NULL, p1 = NULL) {
  if (is.null(p0)) {
    if (p1 < 0 | p1 > 1 ) {
      stop_glue("You input:\n * `p1`: {p1}\n",
                "The prevalences entered must be between 0 and 1.")
    }
  } else if (is.null(p1)) {
    if (p0 < 0 | p0 > 1 ) {
      stop_glue("You input:\n * `p0`: {p0}\n",
                "The prevalences entered must be between 0 and 1.")
    }
  } else if (p1 < 0 | p0 < 0 | p1 > 1 | p0 > 1) {
    stop_glue("You input:\n * `p0`: {p0}\n * `p1`: {p1}\n",
              "The prevalences entered must be between 0 and 1.")
  }
}

tip_gamma <- function(p0 = NULL,
                      p1 = NULL,
                      b = NULL) {
  if (is.null(p1) || is.null(p0)) {
    stop("Please input at least 2 of the following:\n * `p0`\n * `p1`\n * `gamma`",
         call. = FALSE)
  }

  check_prevalences(p0, p1)

  gamma <- ((1 - p1) + b * (p0 - 1)) / (b * p0 - p1)

  if (gamma < 0) {
    stop_glue("Given these prevalences:\n * `p0`: {p0}\n * `p1`: {p1}\n",
              "There does not exist an unmeasured confounder that could tip this.\n",
              "Please specifiy a larger prevalence difference.\n",
              "(ie: make `p0` and `p1` farther apart).")
  }
  as.numeric(gamma)
}

tip_p0 <- function(p1 = NULL,
                   gamma = NULL,
                   b = NULL) {
  if (is.null(p1) || is.null(gamma)) {
    stop("Please input at least 2 of the following:\n * `p0`\n * `p1`\n * `gamma`.",
         call. = FALSE)
  }

  check_prevalences(p1 = p1)
  check_gamma(gamma)

  p0 <- (p1 * (gamma - 1) - b + 1) / (b * (gamma - 1))

  if (p0 > 1 | p0 < 0) {
    stop_glue("Given these parameters:\n * `p1`: {p1}\n * `gamma`: {gamma}\n",
              "There does not exist an unmeasured confounder that could tip this.")
  }
  as.numeric(p0)
}


tip_p1 <- function(p0 = NULL,
                   gamma = NULL,
                   b = NULL) {
  if (is.null(p0) || is.null(gamma)) {
    stop("Please input at least 2 of the following:\n * `p0`\n * `p1`\n * `gamma`.",
         call. = FALSE)
  }

  check_prevalences(p0 = p0)
  check_gamma(gamma)

  p1 <- (b * ((gamma - 1) * p0 + 1) - 1) / (gamma - 1)

  if (p1 > 1 | p1 < 0) {
    stop_glue("Given these parameters:\n * `p0`: {p0}\n * `gamma`: {gamma}\n",
              "There does not exist an unmeasured confounder that could tip this.")
  }
  as.numeric(p1)
}

tip_n <- function(p0, p1, gamma, b) {
  check_prevalences(p0, p1)
  check_gamma(gamma)

  n <- - log(b) / (log(gamma * p0 + (1 - p0)) - log(gamma * p1 + (1 - p1)))
  if (n < 1) {
    warning("This analysis would tip with < 1 of the given unmeasured confounders.",
            call. = FALSE)
  }
  as.numeric(n)
}

e_value <- function(lb, ub) {
  observed_covariate_e_value(lb, ub, 1, 1)
}

hr_transform <- function(hr) {
  (1 - (0.5 ^ sqrt(hr))) / (1 -(0.5 ^ sqrt(1 / hr)))
}
