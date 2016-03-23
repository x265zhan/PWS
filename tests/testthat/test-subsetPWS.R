### This file is written by m1gao

context("subsetting an object of the class 'PWS'")

data(offlineExample)

test_that("subsetPWS returns an object of the class 'PWS'", {
  a = subsetPWS(offlineExample, 2)
  expect_is(a, "PWS")
})

test_that("subsetPWS handles improper inputs", {
  expect_error(subsetPWS(offlineExample, -1), "Invalid distance value!")
  expect_error(subsetPWS("a"))
  expect_error(subsetPWS(2))
  expect_error(subsetPWS(offlineExample, c(1,2)), "Please enter a single distance value!")
})