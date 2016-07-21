#' Latitude and Longitude to Easting and Northing
#'
#' By default convert latitudes and longitudes in the WGS84 geodatum to eastings and northings
#' in BC Albers projection.
#'
#' @param data The data frame or spatial object with the locational information.
#' @param long A string of the name of the column in data with the longitudes.
#' @param lat A string of the name of the column in data with the latitudes.
#' @param east A string of the name of the column to add to data with the eastings.
#' @param north A string of the name of the column to add to data with the northings.
#' @param geodatum A string of the geodatum for the longitudes and latitudes.
#' @param projargs A string of the projection for the eastings and northings.
#' @export
latlong2eastnorth <- function (
  data, long = "Longitude", lat = "Latitude", east = "Easting", north = "Northing",
  geodatum = "WGS84", projargs = "+init=epsg:3005") {

  check_string(long)
  check_string(lat)
  check_string(east)
  check_string(north)
  check_string(geodatum)
  check_string(projargs)

  if (inherits(data, "Spatial")) {
    if (sp::proj4string(data) != paste0("+proj=longlat +ellps=", geodatum))
      stop("data must have projection '+proj=longlat +ellps=WGS84'", call. = FALSE)
    data <- suppressWarnings(broom::tidy(data))
  }
  check_data2(data, values = stats::setNames(list(c(1,NA), c(1,NA)), c(long, lat)))

  if (tibble::has_name(data, east)) warning("column '", east, "' has been replaced", call. = FALSE)
  if (tibble::has_name(data, north)) warning("column '", north, "' has been replaced", call. = FALSE)

  data$Easting <- NA_real_
  data$Northing <- NA_real_

  geodatum <- paste0("+proj=longlat +ellps=", geodatum)
  points <- data[c(long, lat)]
  missing <- is.na(points[[long]]) | is.na(points[[lat]])

  if (any(!missing)) {
    points <- points[!missing,,drop = FALSE]
    points %<>% sp::SpatialPoints(sp::CRS(geodatum))
    points %<>% sp::spTransform(sp::CRS(projargs))
    points <- suppressWarnings(broom::tidy(points)) # SpatialPoints method undefined
    data$Easting[!missing] <- points[[long]]
    data$Northing[!missing] <- points[[lat]]
  }

  data
}

#' Latitude and Longitude to kml File.
#'
#' @inheritParams latlong2eastnorth
#' @param label A string specifying the column with the labels.
#' @param file A string specifying the file name.
#' @export
latlong2kml <- function(data, file = "plot.kml", label = "Site", long = "Longitude", lat = "Latitude", geodatum = "WGS84") {
  check_string(file)
  check_string(label)
  check_string(long)
  check_string(lat)
  check_string(geodatum)

  if (!inherits(data, "Spatial")) {
    check_data1(data, values = stats::setNames(list(c(1,NA), c(1,NA)), c(long, lat)), min_row = 1)

    data[[label]] %<>% as.character()

    data <- data[c(long, lat, label)]
    data %<>% stats::na.omit()
    if (!nrow(data)) stop("data has no rows with non-missing values")

    data %<>% as.data.frame()

    data <- sp::SpatialPointsDataFrame(data[c(long, lat)], data = data[label],
                                       proj4string = sp::CRS(paste0("+proj=longlat +ellps=", geodatum)))
  }
  data %<>% sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))
  plotKML::plotKML(data, points_name = data[[label]])
}
