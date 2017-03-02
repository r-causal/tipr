#' Tipping point
#'
#' @param p1 estimated prevalence of the unmeasured confounder in the exposed population
#' @param p0 estimated prevalence of the unmeasured confounder in the unexposed population
#' @param lb lower bound of your observed effect
#' @param ub upper bound of your observed effect
#' @param explanation logical value indicates whether you'd like the function to output a sentence explanation for inclusion in a manuscript.
#'
#' @return the size of an unmeasured confounder at the given prevalences that would tip the observed result
#'
#' @examples
#' #to output the size of an unmeasured confounder needed to tip analysis
#' tip(p1 = .5,p0 = 0,lb = 1.2,ub = 1.5)
#'
#' #to output a sentence explanation
#' tip(p1 = .5,p0 = 0,lb = 1.2,ub = 1.5,explanation = TRUE)
#'
#' @export
tip <- function(p1, p0, lb = 1, ub = 1, explanation = FALSE) {
  if (lb > 1 & ub > 1){
    b = lb
  } else if (lb < 1 & ub < 1){
    b = ub
  } else stop("Please input a significant result.")
  if(p1 < 0 | p0 < 0 | p1 > 1 | p0 > 1) stop("The prevalences entered must be between 0 and 1")
  gamma = (1-p1+b*(p0-1))/(b*p0-p1)
  if (explanation == TRUE){
    print(paste0("An unmeasured confounder of size ",round(gamma,2), " with a prevalence of ",p1," in the exposed population and ", p0, " in the unexposed population would tip your (",lb,",",ub,") result to nonsignificance."))
  } else gamma
}
