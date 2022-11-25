get_y <- function(m) {
  deparse(stats::formula(m)[[2]])
}

parse_formula <- function(m) {
  as.character(
    attr(m$terms, "variables")
  )[-c(1,2)]

}

create_covariate_lists <- function(ps_mod, outcome_mod) {
  exposure <- get_y(ps_mod)

  ps_covariates <- parse_formula(ps_mod)
  outcome_covariates <- parse_formula(outcome_mod)

  ps_covariates_clean <- unique(clean_covariate(ps_covariates))
  outcome_covariates_clean <- unique(clean_covariate(outcome_covariates))
  outcome_covariates_clean <- outcome_covariates_clean[
    !(outcome_covariates_clean %in% exposure)
  ]
  list(exposure = exposure,
       ps_covariates = ps_covariates,
       ps_covariates_clean = ps_covariates_clean,
       outcome_covariates = outcome_covariates,
       outcome_covariates_clean = outcome_covariates_clean
  )
}

drop_one_mod_tbl <- function(cov, names, covariate_lists) {
  ps_covariates <- covariate_lists[["ps_covariates"]]
  outcome_covariates <- covariate_lists[["outcome_covariates"]]

  cov_ps <- cov[cov %in% covariate_lists[["ps_covariates_clean"]]]
  cov_outcome <- cov[cov %in% covariate_lists[["outcome_covariates_clean"]]]
  if (all(clean_covariate(ps_covariates) %in% cov_ps)) {
    new_ps = 1
  } else{
    new_ps = ps_covariates[
      !(clean_covariate(ps_covariates) %in% cov_ps)
    ]
  }
  tibble::tibble(
    dropped = names,
    new_ps = list(new_ps),
    new_outcome = list(
      outcome_covariates[
        !(clean_covariate(outcome_covariates) %in% cov_outcome)
      ])
  )
}


create_individual_covariate_list <- function(covariate_lists) {
  covs <- as.list(unique(c(covariate_lists[["ps_covariates_clean"]],
                           covariate_lists[["outcome_covariates_clean"]])))
  names(covs) <- covs
  covs
}

drop_tbl <- function(covs, covariate_lists) {

  g <- purrr::map2(covs, names(covs), drop_one_mod_tbl, covariate_lists)
  g <- do.call(rbind, g)
  g$type <- ifelse(purrr::map(covs, length) == 1, "covariate", "group")
  g
}


add_formula <- function(d, exposure, outcome) {
  tibble::add_column(
    d,
    ps_form = purrr::map(d$new_ps, build_formula, y = exposure),
    outcome_form = purrr::map(d$new_outcome, build_formula, y = outcome)
  )
}

clean_covariate <- function(x) {
  gsub(".*\\(|\\).*|\\^.*|,.*$", "", x)
}

build_formula <- function(y, x) {
  covs <- glue::glue_collapse(x, sep = "+")
  stats::as.formula(
    glue::glue("{y} ~ {covs}")
  )
}

check_drop_list <- function(l) {
  if (!is.null(l)) {
    n <- names(l)
    if (length(n) != length(l)) {
      stop_cli("`drop_list` must be a named list.")
    }
    c <- purrr::map_lgl(l, is.character)
    if (!all(c)) {
      stop_cli("`drop_list` must be a named list of character vectors.")
    }
  }
}



