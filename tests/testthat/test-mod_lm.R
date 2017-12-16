context("test-mod_lm.R")

test_that("mod_lm: error if data argument is not data.frame", {
  expect_error(mod_lm(list(a = 1:10)))
})

test_that("mod_lm: error if formula argument is not a formula", {
  expect_error(mod_response(mtcars, mtcars))
})

test_that("mod_lm: returns list of class mod and model_lm", {
  expect_is(mod_lm(mtcars, mpg~cyl), "mod")
})

test_that("mod_lm: returns list of class mod and model_lm", {
  expect_is(mod_lm(mtcars, mpg~cyl), "mod_lm")
})

