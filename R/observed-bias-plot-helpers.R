get_y <- function(m) {
  deparse(stats::formula(m)[[2]])
}

parse_formula <- function(m) {
  as.character(
    attr(m$terms, "variables")
  )[-c(1,2)]
}

drop_group_tbl <- function(grp,
                           ps_covariates,
                           outcome_covariates,
                           ps_covariates_clean,
                           outcome_covariates_clean) {
  grp_ps <- grp[grp %in% ps_covariates_clean]
  grp_outcome <- grp[grp %in% outcome_covariates_clean]
  tibble::tibble(
    new_ps = list(ps_covariates[!(clean_covariate(ps_covariates) %in% grp_ps)]),
    new_outcome = list(outcome_covariates[!(clean_covariate(outcome_covariates) %in% grp_outcome)])
  )
}

drop_cov_tbl <- function(ps_mod, outcome_mod, groups = NULL) {
  exposure <- get_y(ps_mod)

  ps_covariates <- parse_formula(ps_mod)
  outcome_covariates <- parse_formula(outcome_mod)

  ps_covariates_clean <- clean_covariate(ps_covariates)
  outcome_covariates_clean <- clean_covariate(outcome_covariates)
  outcome_covariates_clean <- outcome_covariates_clean[
    !(outcome_covariates_clean %in% exposure)
    ]

  same_ps <- ps_covariates_clean[ps_covariates_clean %in% outcome_covariates_clean]
  same_outcome <- outcome_covariates_clean[outcome_covariates_clean %in% ps_covariates_clean]
  diff_ps <- ps_covariates_clean[!(ps_covariates_clean %in% outcome_covariates_clean)]
  diff_outcome <- outcome_covariates_clean[!(outcome_covariates_clean %in% ps_covariates_clean)]

  same_ps <- same_ps[order(same_ps)]
  same_outcome <- same_outcome[order(same_outcome)]

  drop_tbl <- tibble::tibble(
    ps_drop_clean = c(same_ps, diff_ps, rep(NA_character_, length(diff_outcome))),
    outcome_drop_clean = c(same_outcome, rep(NA_character_, length(diff_ps)), diff_outcome),
    group = FALSE
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

  drop_tbl$new_ps = ifelse(
    is.na(drop_tbl$ps_drop_clean),
    list(ps_covariates),
    drop_tbl$new_ps
  )

  drop_tbl$new_outcome = ifelse(
    is.na(drop_tbl$outcome_drop_clean),
    list(outcome_covariates),
    drop_tbl$new_outcome
  )

  if (!is.null(groups)) {
    g <- purrr::map(groups, drop_group_tbl,
                    ps_covariates = ps_covariates,
                    outcome_covariates = outcome_covariates,
                    ps_covariates_clean = ps_covariates_clean,
                    outcome_covariates_clean = outcome_covariates_clean)
    g <- do.call(rbind, g)
    g <- tibble::add_column(
      g,
      ps_drop_clean = names(groups),
      outcome_drop_clean = names(groups),
      group = TRUE
    )
    drop_tbl <- rbind(drop_tbl, g)
  }

  drop_tbl
}

add_formula <- function(d, exposure, outcome) {
  tibble::add_column(
    d,
    ps_form = purrr::map(d$new_ps, build_formula, y = exposure),
    outcome_form = purrr::map(d$new_outcome, build_formula, y = outcome)
  )
}

clean_covariate <- function(x) {
  gsub(".*\\(|\\).*|\\^.*", "", x)
}

build_formula <- function(y, x) {
  covs <- glue::glue_collapse(x, sep = "+")
  stats::as.formula(
    glue::glue("{y} ~ {covs}")
  )
}

update_model <- function(drop, formula, model) {
  if (is.na(drop)) {
    return(model)
  }
  stats::update(model, formula)
}


check_groups_list <- function(l) {
  if (!is.null(l)) {
    n <- names(l)
    if (length(n) != length(l)) {
      stop_glue("`groups` must be a named list.")
    }
    c <- purrr::map_lgl(l, is.character)
    if (!all(c)) {
      stop_glue("`groups` must be a named list of character vectors.")
    }
  }
}


