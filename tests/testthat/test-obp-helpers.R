context("Observed bias plot helpers")

ps_mod <- glm(am ~ cyl + hp + I(hp^2), data = mtcars, family = "binomial")
outcome_mod <- lm(mpg ~ am + cyl + disp + wt + I(wt^2), data = mtcars)

test_that("Check drop list works", {
  expect_error(check_drop_list("a"), "`drop_list` must be a named list.")

  expect_error(check_drop_list(list("a")), "`drop_list` must be a named list.")

  expect_error(check_drop_list(list(a = 1)), "`drop_list` must be")

  expect_silent(check_drop_list(list(a = "a")))
})

test_that("We can get y from lm or glm formulas", {
  expect_equal(get_y(lm(mpg ~ cyl, data = mtcars)), "mpg")

  expect_equal(get_y(glm(mpg ~ cyl, data = mtcars)), "mpg")

})

test_that("We can get variables from lm or glm formulas", {
  expect_equal(parse_formula(lm(mpg ~ cyl, data = mtcars)), "cyl")

  expect_equal(parse_formula(glm(mpg ~ cyl, data = mtcars)), "cyl")

})

test_that("create_covariate_lists pulls the correct covariates", {
  c <- create_covariate_lists(ps_mod, outcome_mod)

  expect_equal(c$exposure, "am")
  expect_equal(c$ps_covariates, c("cyl", "hp", "I(hp^2)"))
  expect_equal(c$ps_covariates_clean, c("cyl", "hp"))
  expect_equal(c$outcome_covariates, c("am", "cyl", "disp", "wt", "I(wt^2)"))
  expect_equal(c$outcome_covariates_clean, c("cyl", "disp", "wt"))
})

test_that("drop_one_mod_tbl effectively creates a tbl for dropped covariate from both models", {
  t <- drop_one_mod_tbl("cyl", "cyl",
                        create_covariate_lists(ps_mod, outcome_mod))
  expect_equal(t$dropped, "cyl")
  expect_equal(t$new_ps[[1]], c("hp", "I(hp^2)"))
  expect_equal(t$new_outcome[[1]], c("am", "disp", "wt", "I(wt^2)"))

})

test_that("drop_one_mod_tbl effectively creates a tbl for dropped covariate from ps model", {
  t <- drop_one_mod_tbl("hp", "hp", create_covariate_lists(ps_mod, outcome_mod))
  expect_equal(t$dropped, "hp")
  expect_equal(t$new_ps[[1]], c("cyl"))
  expect_equal(t$new_outcome[[1]], c("am", "cyl", "disp", "wt", "I(wt^2)"))

})

test_that("drop_one_mod_tbl effectively creates a tbl for dropped covariate from outcome model", {
  t <- drop_one_mod_tbl("disp", "disp",
                        create_covariate_lists(ps_mod, outcome_mod))
  expect_equal(t$dropped, "disp")
  expect_equal(t$new_ps[[1]], c("cyl", "hp", "I(hp^2)"))
  expect_equal(t$new_outcome[[1]], c("am", "cyl", "wt", "I(wt^2)"))
})

test_that("drop_on_mod_tbl effectively creates a tbl for dropped group of covariates", {
  t <- drop_one_mod_tbl(c("disp", "cyl"), "disp and cyl",
                        create_covariate_lists(ps_mod, outcome_mod))
  expect_equal(t$dropped, "disp and cyl")
  expect_equal(t$new_ps[[1]], c("hp", "I(hp^2)"))
  expect_equal(t$new_outcome[[1]], c("am", "wt", "I(wt^2)"))
})

test_that("create_individual_covariate_list effectively creates a list of all covariates modelled", {
  c <- create_covariate_lists(ps_mod, outcome_mod)
  l <- create_individual_covariate_list(c)
  expect_silent(check_drop_list(l))
  expect_length(l, 4)
  expect_equal(names(l), unlist(l, use.names = FALSE))
})

test_that("drop_tbl creates the appropriate tbl", {
  c <- create_covariate_lists(ps_mod, outcome_mod)
  l <- create_individual_covariate_list(c)
  covs <- c(list("disp and cyl" = c("disp", "cyl")), l)
  t <- drop_tbl(covs, c)
  expect_equal(t[1, "type", drop = TRUE], "group")
  expect_equal(t[-1, "type", drop = TRUE], rep("covariate", 4))
  expect_equal(t$new_ps[[1]], t$new_ps[[2]])
  expect_equal(t$new_ps[[4]], t$new_ps[[5]])
  expect_length(t$new_outcome[[3]], 5)
})

test_that("build_formula works", {
  b <- build_formula("am", c("mpg", "cyl", "I(cyl^2)"))
  expect_s3_class(b, "formula")
  expect_equal(as.character(b[[2]]), "am")
  expect_equal(as.character(b[[3]])[1], "+")
  expect_equal(as.character(b[[3]])[2], "mpg + cyl")
  expect_equal(as.character(b[[3]])[3], "I(cyl^2)")
})

test_that("clean_covariate works", {
  expect_equal(clean_covariate("I(hp^2)"), "hp")
  expect_equal(clean_covariate("rms::rcs(mpg)"), "mpg")
  expect_equal(clean_covariate("rms::rcs(mpg, 3)"), "mpg")
  expect_equal(clean_covariate("sqrt(mpg)"), "mpg")
  expect_equal(clean_covariate("log(mpg)"), "mpg")
  expect_equal(clean_covariate("sqrt(log(mpg))"), "mpg")
  expect_equal(clean_covariate("sqrt(log(mpg, 10))"), "mpg")
  expect_equal(clean_covariate("log(sqrt(mpg), 10))"), "mpg")
})
