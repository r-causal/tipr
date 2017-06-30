#' Tip a result with a binary confounder.
#'
#' choose two of the following three to specify, and the third will be estimated:
#' * `p1`
#' * `p0`
#' * `gamma`
#'
#' [`tip_b()`] is an alias for [`tip_with_binary()`].
#' @param p1 estimated prevalence of the unmeasured confounder in the exposed population
#' @param p0 estimated prevalence of the unmeasured confounder in the unexposed population
#' @param gamma estimated size of an unmeasured confounder
#' @param lb lower bound of your observed effect
#' @param ub upper bound of your observed effect
#' @param explanation logical value indicates whether you'd like the function to output a sentence explanation for inclusion in a manuscript.
#'
#' @return the size of an unmeasured confounder at the given prevalences that would tip the observed result
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

  if (is.null(gamma)){

    if(p1 < 0 | p0 < 0 | p1 > 1 | p0 > 1) stop("The prevalences entered must be between 0 and 1")

    gamma <- ((1-p1)+b*(p0-1))/(b*p0-p1)
    if ({
      # !((p1/p0 > b & b > (1-p1)/(1-p0)) |
      # (p1/p0 < b & b < (1-p1)/(1-p0)))
      gamma < 0
    }) {

      stop(sprintf("Given these prevelances, there does not exist an unmeasured
confounder that could tip this. Please specify a larger prevalence
difference (ie: make p0 and p1 farther apart)."))
    }
    if (explanation == TRUE){
      cat(sprintf("An unmeasured confounder of size %s with a prevalence of %s in the \nexposed population and %s in the unexposed population would tip your \n(%s,%s) result to nonsignificance.",
                  round(gamma,2),p1, p0, lb, ub))
      invisible(gamma)
    } else return(gamma)
  } else if (is.null(p0)){

    if(p1 < 0 | p1 > 1 ) stop("The prevalences entered must be between 0 and 1")


    p0 <- (p1*(-gamma)+p1+b-1)/(b-gamma*b)
    if (p0 > 1 | p0 < 0) {
      stop(sprintf("Given these parameters, there does not exist an unmeasured confounder that could tip this."))
    }
    if (explanation == TRUE){
      cat(sprintf("An unmeasured confounder of size %s with a prevalence of %s in the \nexposed population would need a prevalence of %s in the unexposed population to tip your \n(%s,%s) result to nonsignificance.",
                  round(gamma,2),p1, p0, lb, ub))
      invisible(p0)
    } else return(p0)
  } else if (is.null(p1)){

    if(p0 < 0 | p0 > 1) stop("The prevalences entered must be between 0 and 1")


    p1 <- (b*((gamma-1)*p0+1)-1)/(gamma-1)
    if (p1 > 1 | p1 < 0) {
      stop(sprintf("Given these parameters, there does not exist an unmeasured confounder that could tip this."))
    }
    if (explanation == TRUE){
      cat(sprintf("An unmeasured confounder of size %s with a prevalence of %s in the \nunexposed population would need a prevalence of %s in the exposed population to tip your \n(%s,%s) result to nonsignificance.",
                  round(gamma,2),p0, p1, lb, ub))
      invisible(p1)
    } else return(p1)
  } else warning("Please specify only 2 of the following: p0, p1, gamma. The function will return the third.")
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
