context("latlong2eastnorth")

test_that("latlong2eastnorth error checking", {

  data <- data.frame(Longitude = -131, Latitude = 54)

  expect_error(latlong2eastnorth(data, long = "Latitude"), "column names in values must be unique")
  expect_error(latlong2eastnorth(data, long = 1), "long must be of class 'character'")
  expect_error(latlong2eastnorth(data, long = "Missing2"), "data must have column 'Missing2'")

  data$Easting <- 1
  expect_warning(latlong2eastnorth(data), "column 'Easting' has been replaced")
  data$Easting <- NULL

  data$Longitude <- -131L
  expect_error(latlong2eastnorth(data), "column Longitude in data must be of class 'numeric'")
})

test_that("latlong2eastnorth works", {

  expect_df <- function(x) expect_is(x, "data.frame")

  data <- data.frame(Longitude = -131, Latitude = 54)

  expect_df(datacheckr::check_data3(latlong2eastnorth(data), values = list(
    Longitude = 1,
    Latitude = 1,
    Easting = 1,
    Northing = 1)))

  is.na(data$Longitude) <- TRUE

  expect_df(datacheckr::check_data3(latlong2eastnorth(data), values = list(
    Longitude = c(1,NA),
    Latitude = 1,
    Easting = c(1,NA),
    Northing = c(1,NA))))

  expect_df(datacheckr::check_data2(latlong2eastnorth(haidagwaii::haida_gwaii, long = "long", lat = "lat"), values = list(
    long = 1,
    lat = 1,
    Easting = 1,
    Northing = 1)))
})
