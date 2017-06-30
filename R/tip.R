#' Tip a result with a binary confounder.
#'
#' @description
#' Choose two of the following three to specify, and the third will be estimated:
#' * `p1`
#' * `p0`
#' * `gamma`
#'
#' Alternatively, specify all three and the function will return the number of unmeasured
#' confounders specified needed to tip the analysis.
#'
#' @details [`tip_b()`] is an alias for [`tip_with_binary()`].
#' @param p1 estimated prevalence of the unmeasured confounder in the exposed population
#' @param p0 estimated prevalence of the unmeasured confounder in the unexposed population
#' @param gamma estimated size of an unmeasured confounder
#' @param lb lower bound of your observed effect
#' @param ub upper bound of your observed effect
#' @param explanation logical value indicates whether you'd like the function to output
#'   a sentence explanation for inclusion in a manuscript.
#'
#' @examples
#' #to output the size of an unmeasured confounder needed to tip analysis
#' tip_with_binary(p1 = .5, p0 = 0, lb = 1.2, ub = 1.5)
#'
#' #to output a sentence explanation
#' tip_with_binary(p1 = .5, p0 = 0, lb = 1.2, ub = 1.5, explanation = TRUE)
#'
#' @export
tip_with_binary <- function(p1 = NULL, p0 = NULL, gamma = NULL, lb = NULL, ub = NULL, explanation = FALSE) {
  b <- get_limiting_bound(lb, ub)

  if (is.null(gamma)) {
    return(tip_gamma(p0, p1, b, lb, ub, explanation))
  }

  if (is.null(p0)){
   return(tip_p0(p1, gamma, b, lb, ub, explanation))
  }

  if (is.null(p1)){
    return(tip_p1(p0, gamma, b, lb, ub, explanation))
  }
  tip_n(p0, p1, gamma, b, lb, ub, explanation)
}

#' Tip a result with a continuous confounder.
#'
#' choose one of the following, and the other will be estimated:
#' * `mean_diff`
#' * `gamma`
#'
#' [`tip_c()`] is an alias for [`tip_with_continuous()`].
#'
#' @param mean_diff estimated mean difference of the unmeasured confounder in the
#'  exposed population and unexposed population
#' @param gamma estimated size of an unmeasured confounder
#' @param lb lower bound of your observed effect
#' @param ub upper bound of your observed effect
#' @param explanation logical value indicates whether you'd like the function to
#'  output a sentence explanation for inclusion in a manuscript.
#'
#' @return Numeric. The size of an unmeasured confounder at the given parameters
#'  that would tip the observed result.
#'
#' @examples
#' #to output the size of an unmeasured confounder needed to tip analysis
#' tip_with_continuous(mean_diff = -2, lb = 1.2, ub = 1.5)
#'
#' #to output a sentence explanation
#' tip_with_continuous(mean_diff = -2, lb = 1.2, ub = 1.5, explanation = TRUE)
#'
#' @export
tip_with_continuous <- function(mean_diff = NULL, gamma = NULL, lb = NULL, ub = NULL, explanation = FALSE) {
  b <- get_limiting_bound(lb, ub)

  if (is.null(gamma)){
    gamma <- (1/b)^{1/(mean_diff)}

    if (gamma < 0) {
      stop("This might be wrong..")
    }
    if (explanation == TRUE){
      cat(sprintf("An unmeasured confounder of size %s with a mean difference of %s between \nthe exposed and unexposed population would tip your (%s,%s) result \nto nonsignificance.",
                  round(gamma,2), mean_diff, lb, ub))
      invisible(gamma)
    } else return(gamma)
  } else if (is.null(mean_diff)){
    stop("we can't do this yet")
  }
}

#' @rdname tip_with_binary
#' @export
tip_b <- tip_with_binary

#' @rdname tip_with_continuous
#' @export
tip_c <- tip_with_continuous
