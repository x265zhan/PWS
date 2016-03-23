##
## Begin m1gao code
##

## all S4 classes definitions and their accessor/modifiers


#' An S4 class "PWS"
#'
#' The class of object returned by the \code{\link{getPWS}} function. The PWS class represents the original search location and its nearby PWS (personal weather stations), which are retrieved from the website \url{http://www.wunderground.com}.
#'
#' @section Objects from the Class: Objects can be created by calls of the form new("PWS", ...) or by calling the function \code{\link{getPWS}}.
#'
#' @slot city a single character string that shows the city of the original search location
#' @slot state a single character string that shows the state of the original search location; is character(0) if the origin is outside the U.S.A
#' @slot country a single character string that shows the country of the original search location; is character(0) if the the origin is within the U.S.A
#' @slot lat a numeric that shows the latitude of the original search location; must be between -180 and 180
#' @slot lon a numeric that shows the longitude of the original search location; must be between -180 and 180
#' @slot data a data frame that contains the information of the nearby PWS; in particular, the columns of the data frame should be: \describe{
#' \item{\code{neighborhood}}{a single character string that shows the neighborhood of the PWS}
#' \item{\code{city}}{a single character string that shows the city of the PWS}
#' \item{\code{state}}{a single character string that shows the state of the PWS}
#' \item{\code{country}}{a single character string that shows the country of the PWS}
#' \item{\code{id}}{a numeric that shows the id of the PWS}
#' \item{\code{lat}}{a numeric that shows the latitude of the PWS}
#' \item{\code{lon}}{a numeric that shows the longitude of the PWS}
#' \item{\code{distance_km}}{a positive integer that shows the distance from the PWS to the original search location in kilometers}
#' \item{\code{distance_mi}}{a positive integer that shows the distance from the PWS to the original search location in miles}
#' }
#'
#' @import methods
#' @keywords classes
#' @name PWS-class
#' @rdname PWS-class
#' @note The class \code{\link{PWS-class}} has a validity (see \code{\link{validObject}}) check function.
#' @exportClass PWS
#' @seealso \code{\link{getPWS}}
#' @examples
#' a = getPWS(zipcode = 94305)
#' b = getPWS(city = "San Francisco", state = "CA")
#' validObject(a)
#'
setClass("PWS",
representation(city = "character",
state = "character",
country = "character",
lat = "numeric",
lon = "numeric",
data = "data.frame"))


#' A validation check for S4 class "PWS"
#'
#' @param object an object of class "PWS"
#' @rdname validPWS
#'
.validPWS <- function(object) {
    retVal <- NULL
    if (length(object@city) > 1) {
        retVal <- c(retVal, "city is not a single character string")
    }
    
    if (length(object@state) > 1) {
        retVal <- c(retVal, "state is not a single character string")
    }
    
    if (length(object@country) > 1) {
        retVal <- c(retVal, "country is not a single character string")
    }
    
    if (length(object@lat) > 1) {
        retVal <- c(retVal, "latitude shoud be a single numeric value")
    }
    
    if (length(object@lon) > 1) {
        retVal <- c(retVal, "longitude shoud be a single numeric value")
    }
    
    if (length(object@lat) == 1 && (object@lat < -180 || object@lat > 180)) {
        retVal <- c(retVal, "latitude is not between -180 and 180")
    }
    
    if (length(object@lon) == 1 && (object@lon < -180 || object@lon > 180)) {
        retVal <- c(retVal, "longitude is not between -180 and 180")
    }
    
    if (ncol(object@data) != 0 && is.null(colnames(object@data))) {
        retVal <- c(retVal, "Please enter column names ('neighborhood', 'city', 'state', 'country', 'id', 'lat', 'lon', 'distance_km', 'distance_mi') for the data slot")
    }
    
    if (ncol(object@data) != 0 && !is.null(colnames(object@data))) {
        dfNames <- sort(colnames(object@data))
        expectedNames <- c("city", "country", "distance_km", "distance_mi", "id", "lat", "lon",  "neighborhood", "state")
        
        if (length(colnames(object@data)) != length(expectedNames)) { #if dimension don't match
            retVal <- c(retVal, "Dimension of the data slot is incorrect!")
        }
        
        if (!identical(dfNames, expectedNames)) { #if column names don't match
            retVal <- c(retVal, "Column names of the data slot are incorrect! Please enter column names ('neighborhood', 'city', 'state', 'country', 'id', 'lat', 'lon', 'distance_km', 'distance_mi') for the data slot")
        }
        
        if (identical(dfNames, expectedNames)) { #if column names match
            
            if (class(object@data$neighborhood) != "character" || class(object@data$city) != "character" || class(object@data$state) != "character" || class(object@data$id) != "character" || class(object@data$lat) != "numeric" || class(object@data$lon) != "numeric" || class(object@data$distance_km) != "integer" || class(object@data$distance_mi) != "integer") { #check if the data types match
                retVal <- c(retVal, "at least one column type for the data slot is incorrect!")
            } else if (nrow(object@data)>0 && (min(object@data$lat) < -180 || max(object@data$lat) > 180 || (min(object@data$lon) < -180) || max(object@data$lon) > 180)) { #check if latitude and longitude are between -180 and 180
                retVal <- c(retVal, "at least one latitude/longitude value for the data slot is not between -180 and 180")
            } else if (nrow(object@data)>0 && (min(object@data$distance_km) < 0 || min(object@data$distance_mi) < 0)) { # check if distances are positive
                retVal <- c(retVal, "distance_km/distance_mi for the data slot can't be a negative integer!")
            }
        }
    }
    
    if (is.null(retVal)) {
        return (TRUE)
    } else {
        return (retVal)
    }
}
setValidity("PWS", .validPWS)


#' city accessor
#' @param object object of class \code{PWS}
#' @name getCity
#' @exportMethod getCity
#'
if(!isGeneric("getCity")){
    if (is.function("getCity"))
    fun <- getCity
    else fun <- function(object) standardGeneric("getCity")
    setGeneric("getCity", fun)
}

#' @describeIn PWS returns the city of the original search location
#' @usage NULL
setMethod("getCity", "PWS", function(object) object@city)

#' state accessor
#' @param object object of class \code{PWS}
#' @name getState
#' @exportMethod getState
#'
if(!isGeneric("getState")){
    if (is.function("getState"))
    fun <- getState
    else fun <- function(object) standardGeneric("getState")
    setGeneric("getState", fun)
}

#' @describeIn PWS returns the state of the original search location
#' @usage NULL

setMethod("getState", "PWS", function(object) object@state)

#' country accessor
#' @param object object of class \code{PWS}
#' @name getCountry
#' @exportMethod getCountry
#'
if(!isGeneric("getCountry")){
    if (is.function("getCountry"))
    fun <- getCountry
    else fun <- function(object) standardGeneric("getCountry")
    setGeneric("getCountry", fun)
}

#' @describeIn PWS returns the country of the original search location
#' @usage NULL
setMethod("getCountry", "PWS", function(object) object@country)


#' latitude accessor
#' @param object object of class \code{PWS}
#' @name getLat
#' @exportMethod getLat
#'
if(!isGeneric("getLat")){
    if (is.function("getLat"))
    fun <- getLat
    else fun <- function(object) standardGeneric("getLat")
    setGeneric("getLat", fun)
}

#' @describeIn PWS returns the latitude of the original search location
#' @usage NULL
setMethod("getLat", "PWS", function(object) object@lat)

#' longitude accessor
#' @param object object of class \code{PWS}
#' @name getLon
#' @exportMethod getLon
#'
if(!isGeneric("getLon")){
    if (is.function("getLon"))
    fun <- getLon
    else fun <- function(object) standardGeneric("getLon")
    setGeneric("getLon", fun)
}

#' @describeIn PWS returns the longitude of the original search location
#' @usage NULL
setMethod("getLon", "PWS", function(object) object@lon)

#' data accessor
#' @param object object of class \code{PWS}
#' @name getData
#' @exportMethod getData
#'
if(!isGeneric("getData")){
    if (is.function("getData"))
    fun <- getData
    else fun <- function(object) standardGeneric("getData")
    setGeneric("getData", fun)
}

#' @describeIn PWS returns the data of the original search location
#' @usage NULL
setMethod("getData", "PWS", function(object) object@data)


##
## End m1gao code
##

##
## Begin x265zhan code
##

#' An S4 class "PWSweather"
#'
#' The class of object returned by the \code{\link{getPWSweather}} function. The PWSweather class represents the weather condition of some PWS (personal weather stations) on a given time interval. The data is retrieved from the website \url{http://www.wunderground.com}.
#'
#' @section Objects from the Class: Objects can be created by calls of the form new("PWSweather", ...) or by calling the function \code{\link{getPWSweather}}.
#' 
#' @slot start a single character string in HH:MM or H:MM form that shows the starting time of the time interval
#' @slot end a single character string in HH:MM or H:MM form that shows the ending time of the time interval; must be greater than starting time
#' @slot data a list of data frame that contains the weather of the nearby PWS
#' 
#' @keywords classes
#' @name PWSweather-class
#' @rdname PWSweather-class
#' @note The class \code{\link{PWSweather-class}} has a validity (see \code{\link{validObject}}) check function.
#' @exportClass PWSweather
#' @seealso \code{\link{getPWSweather}}
#' @examples
#' data(offlineExample)
#' a = getPWSweather(offlineExample, "3:00", "17:00")
#' validObject(a)
#'
setClass("PWSweather",
         representation(data = "list",
                        start = "character",
                        end = "character"))

#' A validation check for S4 class "PWSweather"
#' 
#' @param object an object of class "PWSweather"
#' @rdname validPWSweather
#' 
.validPWSweather <- function(object) {
  retVal <- NULL
  if (length(object@start) > 1) {
    retVal <- c(retVal, "start time is not a single character string")
  }
  if (length(object@end) > 1) {
    retVal <- c(retVal, "end time is not a single character string")
  }
  if (length(object@start) == 1 && is.na(strptime(object@start, format = "%H:%M"))) {
    retVal <- c(retVal, "start time should be valid time in the form of HH:MM")
  }
  if (length(object@start) == 1 && is.na(strptime(object@end, format = "%H:%M"))) {
    retVal <- c(retVal, "end time should be valid time in the form of HH:MM")
  }
  if (!is.list(object@data) || !all(unlist(lapply(object@data, is.data.frame)))) {
    retVal <- c(retVal, "data should be a list of data frame")
  }
  if (!is.na(strptime(object@start, format = "%H:%M")) && !is.na(strptime(object@end, format = "%H:%M")) &&
      strptime(object@start, format = "%H:%M") > strptime(object@end, format = "%H:%M")) {
    retVal <- c(retVal, "ending time should be larger than starting time")
  }
  if (is.null(retVal)) {
    return (TRUE)
  } else {
    return (retVal)
  }
}
setValidity("PWSweather", .validPWSweather)

#' data accessor
#' @param object object of class \code{PWSweather}
#' @name getWeatherData
#' @exportMethod getWeatherData
#' 
setGeneric("getWeatherData", function(object) standardGeneric("getWeatherData"))

#' @describeIn PWSweather returns the weather data of the nearby stations
#' @usage NULL
setMethod("getWeatherData", "PWSweather", function(object) { object@data })

#' start accessor
#' @param object object of class \code{PWSweather}
#' @name getStartTime
#' @exportMethod getStartTime
#' 
setGeneric("getStartTime", function(object) standardGeneric("getStartTime"))

#' @describeIn PWSweather returns the starting time of the time interval
#' @usage NULL
setMethod("getStartTime", "PWSweather", function(object) { object@start })

#' end accessor
#' @param object object of class \code{PWSweather}
#' @name getEndTime
#' @exportMethod getEndTime
#' 
setGeneric("getEndTime", function(object) standardGeneric("getEndTime"))

#' @describeIn PWSweather returns the ending time of the time interval
#' @usage NULL
setMethod("getEndTime", "PWSweather", function(object) { object@end })

##
## End x265zhan code
##
