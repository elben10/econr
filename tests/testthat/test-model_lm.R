context("test-model_lm.R")

test_that("model_lm: error if data argument is not data.frame", {
  expect_error(model_lm(list(a = 1:10)))
})

test_that("model_lm: error if formula argument is not a formula", {
  expect_error(model_response(mtcars, mtcars))
})

test_that("model_lm: returns list of class mod and model_lm", {
  expect_is(model_lm(mtcars, mpg~cyl), "mod")
})

test_that("model_lm: returns list of class mod and model_lm", {
  expect_is(model_lm(mtcars, mpg~cyl), "model_lm")
})

