get_limiting_bound <- function(lb = NULL, ub = NULL) {
  if (is.null(lb) || is.null(ub)) {
    stop("Please input values for your observed confidence interval (`lb`, `ub`).",
         call. = FALSE)
  }
  if (lb < 0 || ub < 0) {
    spf("You input: (%s, %s)\nWe are expecting an Odds Ratio, Hazard Ratio, or Relative Risk,\ntherefore `lb` or ub` should not be less than 0.",
        lb, ub)
  }
  if (lb > 1 && ub > 1) {
    return(c(lb = lb))
  }
  if (lb < 1 && ub < 1) {
    return(c(ub = ub))
  }
  spf("You input: (%s, %s)\nPlease input a significant result.",
      lb, ub)
}

tip_gamma <- function(p0 = NULL,
                      p1 = NULL,
                      b = NULL,
                      lb = NULL,
                      ub = NULL) {
  if (is.null(p1) || is.null(p0)) {
    stop("Please input at least 2 of the following:\n`p0`,`p1`,`gamma`.")
  }

  if (p1 < 0 | p0 < 0 | p1 > 1 | p0 > 1) {
    spf("You input: `p0`: %s, `p1`: %s\nThe prevalences entered must be between 0 and 1",
        p0, p1)
  }

  gamma <- ((1 - p1) + b*(p0 - 1)) / (b*p0 - p1)
  if (gamma < 0) {

    spf("Given these prevelances, there does not exist an unmeasured
                 confounder that could tip this. Please specify a larger prevalence
                 difference (ie: make p0 and p1 farther apart).")
  }
  as.numeric(gamma)
}

tip_p0 <- function(p1 = NULL,
                   gamma = NULL,
                   b = NULL,
                   lb = NULL,
                   ub = NULL) {
  if (is.null(p1) || is.null(gamma)) {
    stop("Please input at least 2 of the following:\n`p0`,`p1`,`gamma`.")
  }
  if (p1 < 0 | p1 > 1 ) {
    spf("You input: `p1`: %s\nThe prevalences entered must be between 0 and 1",
        p1)
  }

  p0 <- (p1*( - gamma) + p1 + b - 1)/(b - gamma*b)

  if (p0 > 1 | p0 < 0) {
    spf("Given these parameters: `p1`: %s, `gamma`: %s, `lb`: %s, `ub`: %s\nthere does not exist an unmeasured confounder that could tip this.",
        p1, gamma, lb, ub)
  }
  as.numeric(p0)
}


tip_p1 <- function(p0 = NULL,
                   gamma = NULL,
                   b = NULL,
                   lb = NULL,
                   ub = NULL) {
  if (is.null(p0) || is.null(gamma)) {
    stop("Please input at least 2 of the following:\n`p0`,`p1`,`gamma`.")
  }
  if(p0 < 0 | p0 > 1) {
    spf("You input: `p0`: %s\nThe prevalences entered must be between 0 and 1",
        p0)
  }

  p1 <- (b*((gamma - 1)*p0 + 1) - 1)/(gamma - 1)

  if (p1 > 1 | p1 < 0) {
    spf("Given these parameters: `p0`: %s, `gamma`: %s, `lb`: %s, `ub`: %s\nthere does not exist an unmeasured confounder that could tip this.",
        p0, gamma, lb, ub)
  }
  as.numeric(p1)
}

tip_n <- function(p0, p1, gamma, b, lb, ub) {
  n <- log(1/b)/(log((gamma*p0+(1-p0))/(gamma*p1+(1-p1))))
  if (n < 1) {
    warning("This analysis would tip with < 1 of the given unmeasured confounders.",
            call. = FALSE)
  }
  as.numeric(n)
}

e_value <- function(rr) {
  rr + sqrt(rr * (rr - 1))
}
