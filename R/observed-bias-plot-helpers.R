drop_cov_tbl <- function(exposure, ps_covariates, outcome_covariates) {

  outcome_covariates_clean <- clean_covariate(
    outcome_covariates[!(outcome_covariates %in% exposure)])
  ps_covariates_clean <- clean_covariate(ps_covariates)

  same_ps <- ps_covariates_clean[ps_covariates_clean %in% outcome_covariates_clean]
  same_outcome <- outcome_covariates_clean[outcome_covariates_clean %in% ps_covariates_clean]
  diff_ps <- ps_covariates_clean[!(ps_covariates_clean %in% outcome_covariates_clean)]
  diff_outcome <- outcome_covariates_clean[!(outcome_covariates_clean %in% ps_covariates_clean)]

  same_ps <- unique(same_ps[order(same_ps)])
  same_outcome <- unique(same_outcome[order(same_outcome)])

  drop_tbl <- tibble::tibble(
    ps_drop_clean = c(same_ps, diff_ps, rep(NA_character_, length(diff_outcome))),
    outcome_drop_clean = c(same_outcome, rep(NA_character_, length(diff_ps)), diff_outcome)
  )

  drop_tbl <- tibble::add_column(
    drop_tbl,
    new_ps = purrr::map(
      drop_tbl$ps_drop_clean,
      ~ ps_covariates[.x != clean_covariate(ps_covariates)]
    ),
    new_outcome = purrr::map(
      drop_tbl$outcome_drop_clean,
      ~ outcome_covariates[.x != clean_covariate(outcome_covariates)]
    )
  )

  drop_tbl <- tibble::add_column(
    drop_tbl,
    new_outcome = ifelse(
      is.na(drop_tbl$outcome_drop_clean),
      list(outcome_covariates),
      drop_tbl$new_outcome
    ),
    new_ps = ifelse(
      is.na(drop_tbl$ps_drop_clean),
      list(ps_covariates),
      drop_tbl$new_ps
    )
  )
  drop_tbl
}

add_formula <- function(d, exposure, outcome) {
  tibble::add_column(
    d,
    ps_form = purrr::map(d$new_ps, build_formula, y = exposure),
    outcome_form = purrr::map(dl$new_outcome, build_formula, y = outcome)
  )
}
d <- add_formula(drop_tbl, "am", "y")

clean_covariate <- function(x) {
  gsub(".*\\(|\\).*|\\^.*", "", x)
}

build_formula <- function(y, x) {
  covs <- glue::glue_collapse(x, sep = "+")
  as.formula(
    glue::glue("{y}~{covs}")
  )
}

update_model <- function(drop, formula, model) {
  if (is.na(drop)) {
    return(model)
  }
  update(model, formula)
}

update_ps <- function(ps_mod, outcome_mod) {

  drop_cov
}
ps_mod <- glm(am ~ mpg + cyl + I(hp^2), data = mtcars)
outcome_mod <- lm(qsec ~ am + hp + disp + wt, data = mtcars)

d <- drop_cov_tbl("am", c("mpg", "cyl", "I(hp^2)"), c("am", "hp", "disp", "wt"))
d <- add_formula(d, "am", "qsec")
d$new_outcome_mod <- purrr::map2(d$outcome_drop_clean, d$outcome_form, ~ update_model(.x, .y, outcome_mod))
d$new_ps_mod <- purrr::map2(d$ps_drop_clean, d$ps_form, ~ update_model(.x, .y, ps_mod))
