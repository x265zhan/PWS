### This file is written by x265zhan

data(offlineExample)
context("Testing PWSweather class")
context("creating an object of the class 'PWSweather' through new()")

test_that("Invalid data input", {
  expect_error(new("PWSweather", data = "call me general PWSweather", start = "1:00", end = "4:00"))
  expect_error(new("PWSweather", data = list('data1'), start = "1:00", end = "4:00"),
               "data should be a list of data frame")
  expect_is(new("PWSweather", data = list(data.frame()), start = "1:00", end = "4:00"), "PWSweather")
  expect_error(new("PWSweather", data = list(data.frame(), 'data1'), start = "1:00", end = "4:00"),
               "data should be a list of data frame")
})

test_that("Invalid start time input", {
  expect_error(new("PWSweather", data = list(data.frame()), start = "one", end = "4:00"),
               "start time should be valid time in the form of HH:MM")
  expect_error(new("PWSweather", data = list(data.frame()), start = "1", end = "4:00"),
               "start time should be valid time in the form of HH:MM")
  expect_error(new("PWSweather", data = list(data.frame()), start = c(1,2), end = "4:00"),
               "character")
  expect_error(new("PWSweather", data = list(data.frame()), start = list('1:00','1:25'), end = "4:00"),
               "character")
})

test_that("Invalid end time input", {
  expect_error(new("PWSweather", data = list(data.frame()), end = "one", start = "4:00"),
               "end time should be valid time in the form of HH:MM")
  expect_error(new("PWSweather", data = list(data.frame()), end = "1", start = "4:00"),
               "end time should be valid time in the form of HH:MM")
  expect_error(new("PWSweather", data = list(data.frame()), end = c(1,2), start = "4:00"),
               "character")
  expect_error(new("PWSweather", data = list(data.frame()), end = list('1:00','1:25'), start = "4:00"),
               "character")
})

test_that("start time before end time", {
  expect_error(new("PWSweather", data = list(data.frame()), end = "1:00", start = "4:00"),
               "ending time should be larger than starting time")
})