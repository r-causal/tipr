#' Tidy tip a result with unmeasured confounding.
#'
#' @param mod Model object from [`glm()`] or [`coxph()`]
#' @param exposure Character string. The name of the exposure variable.
#' @param rr_eu Numeric. The association between the exposure and the unmeasured confounder.
#' @param rr_ud Numeric. The association between the unmeasured confounder and the outcome.
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#' ## Fit a model
#' mod <- glm(vs ~ mpg, data = mtcars, family = binomial())
#'
#' ## Solve for the association between the exposure and unmeasured
#' ## confounder needed to tip the analysis
#' mod %>%
#'  tip_mod(exposure = "mpg",
#'      rr_ud = 1.4)
#'
#' ## Solve for the association between the unmeasured confounder
#' ## and the outcome needed to tip the analysis
#' mod %>%
#'  tip_mod(exposure = "mpg",
#'      rr_eu = 1.4)
#' ## Solve for the minimum joint association between the
#' ## exposure and unmeasured confounder and the unmeasured confounder
#' ## and outcome, also known as the E-value
#' mod %>%
#'   tip_mod(exposure = "mpg")
#'}
tip_mod <- function(mod, exposure, rr_eu = NULL, rr_ud = NULL) {

  df <- broom::tidy(mod, conf.int = 0.95)
  df <- df[df$term == exposure, ]
  lb <- exp(df[["conf.low"]])
  ub <- exp(df[["conf.high"]])

  b <- get_limiting_bound(lb, ub)
  if (names(b) == "ub") {
    b <- 1/b
  }
  n <- 1
  if (!is.null(rr_eu) & !is.null(rr_ud)) {
    n <- tip_n(p0 = 1/rr_eu, p1 = 1, gamma = rr_ud, b, lb, ub)
  }

  if (is.null(rr_ud) && is.null(rr_eu)) {
    rr_ud <- rr_eu <- e_value(b)
  } else if (is.null(rr_ud)) {
    rr_ud <- tip_gamma(p0 = 1/rr_eu, p1 = 1, b, lb, ub)
  } else if (is.null(rr_eu)){
    rr_eu <- 1 / tip_p0(p1 = 1, gamma = rr_ud, b, lb, ub)
  }

  tibble::tibble(
    lb = lb,
    ub = ub,
    rr_eu = rr_eu,
    rr_ud = rr_ud,
    n = n
  )
}
