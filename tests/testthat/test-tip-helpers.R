context("Tip Helpers")

test_that("get_limiting_bound() errors if not significant", {
  expect_error(get_limiting_bound(lb = .9, ub = 1.1),
               "Please input a significant result")

  expect_error(get_limiting_bound(lb = 1.1, ub = .9),
               "Please input a significant result")

  expect_error(get_limiting_bound(lb = 1, ub = 1.1),
               "Please input a significant result")

  expect_error(get_limiting_bound(lb = 1, ub = 1),
               "Please input a significant result")
})

test_that("get_limiting_bound() errors if lb or ub < 0", {
  expect_error(get_limiting_bound(lb = .9, ub = -1))
  expect_error(get_limiting_bound(lb = -1, ub = .9))
  expect_error(get_limiting_bound(lb = -1, ub = -1))
})

test_that("get_limiting_bound() gives correct bound", {

  expect_identical(get_limiting_bound(lb = 1.1, ub = 1.2), 1.1)
  expect_identical(get_limiting_bound(lb = 0.8, ub = 0.9), 0.9)
  expect_identical(get_limiting_bound(lb = 1.1, ub = 1.1), 1.1)

})
