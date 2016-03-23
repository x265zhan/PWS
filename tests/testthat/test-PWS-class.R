### This file is written by m1gao

context("creating an object of the class 'PWS' through new()")

test_that("valid check for the city slot", {
  expect_error(new("PWS", city = 123))
  expect_error(new("PWS", city = c("San Diego", "San Antonio")), "city is not a single character string")
})

test_that("valid check for the state slot", {
  expect_error(new("PWS", state = 123))
  expect_error(new("PWS", state = c("CA", "SA")), "state is not a single character string")
})

test_that("valid check for the country slot", {
  expect_error(new("PWS", country = 123))
  expect_error(new("PWS", country = c("USA", "China")), "country is not a single character string")
})

test_that("valid check for the latitude slot", {
  expect_error(new("PWS", lat = 183), "latitude is not between -180 and 180")
  expect_error(new("PWS", lat = c(123, 189)), "latitude shoud be a single numeric value")
})

test_that("valid check for the longitude slot", {
  expect_error(new("PWS", lon = -183), "longitude is not between -180 and 180")
  expect_error(new("PWS", lon = c(-123, 189)), "longitude shoud be a single numeric value")
})

test_that("valid check for the data slot", {
  expect_error(new("PWS", data = data.frame(1, 2, 3)), "Dimension of the data slot is incorrect!")  
  expect_error(new("PWS", data = data.frame(1, 2, 3, 4, 5, 6, 7, 8, 9)), "Column names") 
  expect_error(new("PWS", data = data.frame('neighborhood'= 'abc', 'city' = 'San Diego', 'state' = 'CA', 'country' = 'USA', 'id' = 12345, 'lat' = 123, 'lon' = -123, 'distance_km' = 1L, 'distance_mi' = 2L, stringsAsFactors = FALSE)), "at least one column type for the data slot is incorrect!") 
  expect_error(new("PWS", data = data.frame('neighborhood'= 'abc', 'city' = 'San Diego', 'state' = 'CA', 'country' = 'USA', 'id' = '12345abc', 'lat' = -200, 'lon' = -123, 'distance_km' = 1L, 'distance_mi' = 2L, stringsAsFactors = FALSE)), "at least one latitude/longitude value for the data slot is not between -180 and 180") 
  expect_error(new("PWS", data = data.frame('neighborhood'= 'abc', 'city' = 'San Diego', 'state' = 'CA', 'country' = 'USA', 'id' = '12345abc', 'lat' = 100, 'lon' = -123, 'distance_km' = 1L, 'distance_mi' = -2L, stringsAsFactors = FALSE)), "distance_km/distance_mi for the data slot can't be a negative integer!") 
})

