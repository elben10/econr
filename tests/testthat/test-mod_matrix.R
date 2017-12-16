context("test-model_matrix.R")

# MODEL MATRIX

test_that("error if data argument is not data.frame", {
  expect_error(mod_matrix(list(a = 1:10)))
})

test_that("error if formula argument is not a formula", {
  expect_error(mod_matrix(mtcars, mtcars))
})

test_that("model_matrix returns tibble", {
  expect_is(mod_matrix(mtcars, mpg~cyl), "data.frame")
})

# MODEL RESPONSE

test_that("error if data argument is not data.frame", {
  expect_error(mod_response(list(a = 1:10)))
})

test_that("error if formula argument is not a formula", {
  expect_error(mod_response(mtcars, mtcars))
})

test_that("model_matrix returns tibble", {
  expect_is(mod_response(mtcars, mpg~cyl), "numeric")
})
