### This file is written by x265zhan

data(offlineExample)
context("Testing function getPWSweather")
context("check Input Type")
test_that("invalid pws object input",{
  expect_error(getPWSweather('iam pws', "1:00", "4:00"), "Input should be a valid PWS object")
})

test_that("invalid start time input",{
  expect_error(getPWSweather(offlineExample, c("0:30","1:00"), "4:00"), "start time is not a single character string")
  expect_error(getPWSweather(offlineExample, list("0:30","1:00"), "4:00"), "start time is not a single character string")
  expect_error(getPWSweather(offlineExample, "lets start", "4:00"), "start time should be valid time in the form of HH:MM")
  expect_error(getPWSweather(offlineExample, 12, "4:00"), "start time should be valid time in the form of HH:MM")
  expect_error(getPWSweather(offlineExample, 2.5, "4:00"), "start time should be valid time in the form of HH:MM")
  expect_error(getPWSweather(offlineExample, TRUE, "4:00"), "start time should be valid time in the form of HH:MM")
})

test_that("invalid end time input",{
  expect_error(getPWSweather(offlineExample, "0:00", c("0:30","1:00")), "end time is not a single character string")
  expect_error(getPWSweather(offlineExample, "0:00", list("0:30","1:00")), "end time is not a single character string")
  expect_error(getPWSweather(offlineExample, "4:00", "Done"), "end time should be valid time in the form of HH:MM")
  expect_error(getPWSweather(offlineExample, "4:00", 2.5), "end time should be valid time in the form of HH:MM")
  expect_error(getPWSweather(offlineExample, "4:00", TRUE), "end time should be valid time in the form of HH:MM")
  expect_error(getPWSweather(offlineExample, "4:00", 12), "end time should be valid time in the form of HH:MM")
})

context("Check Output Info")

test_that("Return an object of class 'PWSweather'", {
  a = getPWSweather(offlineExample, '1:00', '4:00')
  expect_is(a, 'PWSweather')
})