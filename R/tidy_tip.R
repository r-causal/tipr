#' Tidy tip a result with a binary confounder.
#'
#' @param mod Model object from [`glm()`] or [`coxph()`]
#' @param exposure Character string. The name of the exposure variable.
#' @param p0 Numeric. Estimated prevalence of the unmeasured confounder in the
#'   unexposed population.
#' @param p1 Numeric. Estimated prevalence of the unmeasured confounder in the
#'   exposed population.
#' @param gamma Numeric. Estimated size of an unmeasured confounder.
#' @param explanation Logical. Indicates whether you'd like the function to output
#'   a sentence explanation for inclusion in a manuscript.
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' ## Fit a model
#' mod <- glm(vs ~ mpg, data = mtcars, family = binomial())
#'
#' ## Calculate p1
#' mod %>%
#'   tidy_tip(exposure = "mpg",
#'            p0 = 0,
#'            gamma = 1.4)
#'
#' ## Calculate p0
#' mod %>%
#'   tidy_tip(exposure = "mpg",
#'            p1 = 0,
#'            gamma = .71)
#'
#' ## Calculate gamma
#' mod %>%
#'   tidy_tip(exposure = "mpg",
#'            p0 = 0,
#'            p1 = 0.1)
#'
#' ## Calculate n
#' mod %>%
#'   tidy_tip(exposure = "mpg",
#'            p0 = 0,
#'            p1 = 0.1,
#'            gamma = 1.1)
#'}
tidy_tip <- function(mod, exposure, p0 = NULL, p1 = NULL, gamma = NULL, explanation = FALSE) {

  df <- broom::tidy(mod, conf.int = 0.95)
  df <- df[df$term == exposure, ]
  lb <- exp(df[["conf.low"]])
  ub <- exp(df[["conf.high"]])

  b <- get_limiting_bound(lb, ub)
  n <- 1
  if (!is.null(gamma) & !is.null(p0) & !is.null(p1)) {
    n <- tip_n(p0, p1, gamma, b, lb, ub, explanation)
  }

  if (is.null(gamma)) {
    gamma <- tip_gamma(p0, p1, b, lb, ub, explanation)
  }

  if (is.null(p0)){
    p0 <- tip_p0(p1, gamma, b, lb, ub, explanation)
  }

  if (is.null(p1)){
    p1 <- tip_p1(p0, gamma, b, lb, ub, explanation)
  }
  tibble::tibble(
    lb = lb,
    ub = ub,
    p0 = p0,
    p1 = p1,
    gamma = gamma,
    n = n
  )
}
