get_limiting_bound <- function(lb, ub) {
  if (lb < 0 || ub < 0) {
    spf("You input: (%s, %s)\nWe are expecting an Odds Ratio, Hazard Ratio, or Relative Risk,\ntherefore `lb` or ub` should not be less than 0.",
        lb, ub)
  }
  if (lb > 1 && ub > 1) {
    return(lb)
  }
  if (lb < 1 && ub < 1) {
    return(ub)
  }
  spf("You input: (%s, %s)\nPlease input a significant result.",
      lb, ub)
}
