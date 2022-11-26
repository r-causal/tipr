context("Tip Helpers")

test_that("get_limiting_bound() errors if not significant", {
  expect_error(
    get_limiting_bound(lb = .9, ub = 1.1),
    "Please input a significant result"
  )

  expect_error(
    get_limiting_bound(lb = 1.1, ub = .9),
    "Please input a significant result"
  )

  expect_error(
    get_limiting_bound(lb = 1, ub = 1.1),
    "Please input a significant result"
  )

  expect_error(
    get_limiting_bound(lb = 1, ub = 1),
    "Please input a significant result"
  )
  expect_error(
    get_limiting_bound(),
    "Please input a dataset `d`"
  )
})

test_that("get_limiting_bound() errors if lb or ub < 0", {
  expect_error(get_limiting_bound(lb = .9, ub = -1))
  expect_error(get_limiting_bound(lb = -1, ub = .9))
  expect_error(get_limiting_bound(lb = -1, ub = -1))
})

test_that("get_limiting_bound() gives correct bound", {
  expect_equivalent(get_limiting_bound(lb = 1.1, ub = 1.2), 1.1)
  expect_equivalent(get_limiting_bound(lb = 0.8, ub = 0.9), 0.9)
  expect_equivalent(get_limiting_bound(lb = 1.1, ub = 1.1), 1.1)
})

test_that("tip_gamma() errors when necessary", {
  expect_error(
    tip_gamma(p0 = -1, p1 = 1),
    "The prevalences entered must be between 0 and 1"
  )
  expect_error(
    tip_gamma(p0 = 1, p1 = -1),
    "The prevalences entered must be between 0 and 1"
  )
})

test_that("tip_gamma() returns correct result", {
  expect_identical(tip_gamma(p0 = 0, p1 = 1, b = 1.2), 1.2)
  expect_identical(tip_gamma(p0 = 0, p1 = 1, b = .8), .8)
  expect_identical(tip_gamma(p0 = 1, p1 = 0, b = 1.2), 1 / 1.2)
  expect_error(
    tip_gamma(p0 = .5, p1 = .2, b = 5),
    "there does not exist an unmeasured"
  )
})
