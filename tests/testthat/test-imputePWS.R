### This file is written by x265zhan
data(offlineWeather)

context("Testing imputePWS")

test_that("Invalid input variable", {
  expect_error(imputePWS(offlineWeather, 'Temperature'), "undefined columns selected")
  expect_error(imputePWS(offlineWeather, 'Tempi'), "undefined columns selected")
})

test_that("Output class is a data frame with a timestamp column", {
  a <- imputePWS(offlineWeather, 'tempi')
  expect_true('time' %in% names(a))
  expect_false('times' %in% names(a))
})
