### This file is written by m1gao

context("getting an object of the class 'PWS' by function 'getPWS'")

test_that("getPWS handles improper latitude/longitude inputs", {
  expect_error(getPWS(lat = 100, zipcode = 94305), "Please enter a longitude value")
  expect_error(getPWS(lat = 140, lon = -200), "Invalid latitude/longitude value")
  expect_error(getPWS(lat = c(NA, 120), lon = 150), "Please enter a single numeric value for latitude/longitude")
})

test_that("getPWS handles improper zipcode input", {
  expect_error(getPWS(zipcode = 1234), "Please enter a valid zipcode")
  expect_error(getPWS(zipcode = 123456), "Please enter a valid zipcode")
  expect_error(getPWS(zipcode = "abc"), "Please enter a valid zipcode")
  expect_error(getPWS(zipcode = c(12345, 54321)), "Please enter a single zipcode")
})

test_that("getPWS handles improper city input", {
  expect_error(getPWS(city = ""), "Invalid city name!")
  expect_error(getPWS(city = 123), "Invalid city name!")
  expect_error(getPWS(city = c("La Jolla", "Paris"), state = "CA"), "Please enter a single city!")
  expect_error(getPWS(city = "La Jolla"), "Please enter a state or a country!")
  expect_error(getPWS(city = "La Jolla", state = "CAA"), "Invalid state name!")
  expect_error(getPWS(city = "La Jolla", state = 123), "Invalid state name!")
  expect_error(getPWS(city = "La Jolla", state = c(NA, "NY")), "Please enter a single state")
  expect_error(getPWS(city = "Paris", country = 123), "Invalid country name!")
  expect_error(getPWS(city = "Paris", country = c("Paris", "China")), "Please enter a single country")
})

test_that("getPWS returns an object of class 'PWS'", {
  a = getPWS(zipcode = 94305)
  expect_is(a, "PWS")
})
