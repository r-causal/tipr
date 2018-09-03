#' Order observed bias data frame for plotting
#'
#' @param d Observed bias data frame. Must have columns `dropped` and `group`
#' @param by Character. Variable in `d` to order by.
#'
#' @return Data frame in the correct order
#' @export
observed_bias_order <- function(d, by) {
  grps_ <- d[d$group & !grepl("Hypothetical", d$dropped), ]
  grps <- which(d$group & !grepl("Hypothetical", d$dropped))
  grps <- grps[order(grps_[[by]], decreasing = TRUE)]

  hypo_ <- d[d$group & grepl("Hypothetical", d$dropped), ]
  hypo <- which(d$group & grepl("Hypothetical", d$dropped))
  hypo <- hypo[order(hypo_[[by]])]

  d <- d[c(hypo, grps, order(d[[by]][!d$group], decreasing = TRUE)), ]
  d$dropped <- factor(d$dropped,
                      levels = d$dropped)
  d
}
