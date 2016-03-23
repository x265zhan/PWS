## This File is Written by m1gao

#' get an object of class "PWS"
#' 
#' Gets the nearby PWS (personal weather station) from \url{http://www.wunderground.com} based on a original search location and a distance range. 
#' 
#' Three methods of specifying a original search location are defined. The function first searches the location by \code{lat} and \code{lon} if they are supplied. If \code{lat} and \code{lon} are not supplied, the function next searches the location by the \code{zipcode} if it is supplied. If again \code{zipcode} is not supplied, then the function searches the location by the \code{city}. If \code{state} is supplied, then the city will be searched within the USA. Otherwise if \code{country} is supplied, then the city will be searched outside the USA.
#' The function then gets the nearby PWS of the original search location within a distance range which is specified by \code{distance}.      
#' 
#' @param lat a numeric that specifies the latitude of the original search location; must be between -180 and 180
#' @param lon a numeric that specifies the longitude of the original search location; must be between -180 and 180
#' @param zipcode a numeric that specifies the zipcode of the original search location; only valid for locations within USA
#' @param city a single character string that specifies the city of the original search location
#' @param state a single character string that specifies the state of the original search location; should be supplied if the location is within USA
#' @param country a single character string that specifies the country of the original search location; should be supplied if the location is outside USA
#' @param distance a positive numeric which specifies the distance range of the nearby PWS from the original search location
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON 
#' 
#' @return An S4 object of class \code{\link{PWS-class}} containing the information about the original search location and the nearby PWS. The object contains the following slots:
#' \describe{
#' \item{\code{city}}{a single character string that shows the city of the original search location}
#' \item{\code{state}}{a single character string that shows the state of the original search location; is character(0) if the origin is outside the U.S.A}
#' \item{\code{country}}{a single character string that shows the country of the original search location; is character(0) if the the origin is within the U.S.A}
#' \item{\code{lat}}{a numeric that shows the latitude of the original search location; must be between -180 and 180}
#' \item{\code{lon}}{a numeric that shows the longitude of the original search location; must be between -180 and 180}
#' \item{\code{data}}{a data frame that contains the information of the nearby PWS; in particular, the columns of the data frame should be: \describe{
#' \item{\code{neighborhood}}{a single character string that shows the neighborhood of the PWS}
#' \item{\code{city}}{a single character string that shows the city of the PWS}
#' \item{\code{state}}{a single character string that shows the state of the PWS}
#' \item{\code{country}}{a single character string that shows the country of the PWS}
#' \item{\code{id}}{a numeric that shows the id of the PWS}
#' \item{\code{lat}}{a numeric that shows the latitude of the PWS}
#' \item{\code{lon}}{a numeric that shows the longitude of the PWS}
#' \item{\code{distance_km}}{a positive integer that shows the distance from the PWS to the original search location in kilometers}
#' \item{\code{distance_mi}}{a positive integer that shows the distance from the PWS to the original search location in miles}
#' }}}
#' see \code{\link{PWS-class}} for more details
#' @name getPWS
#' @rdname getPWS
#' @export
#' @seealso \code{\link{PWS-class}}
#' @examples
#' a = getPWS(zipcode = 94305)
#' b = getPWS(city = "San Francisco", state = "CA")
#' c = getPWS(lat = 37, lon = -120)
#' 
utils::globalVariables(c("distance_km"))

getPWS <- function(lat = NA, lon = NA, zipcode = NA, city = NA, state = NA, country = NA, distance = 50) {
  key <- .getAPI() #generates an api key
  baseurl <- paste0("http://api.wunderground.com/api/", key, "/geolookup/q/")
  url <- ""
  if (length(distance) > 1 || !is.numeric(distance) || distance < 0) stop("Invalid distance value")
  
  if (length(lat) > 1 || length(lon) > 1) stop ("Please enter a single numeric value for latitude/longitude")
  
  if (!is.na(lat) && is.na(lon)) {
    stop("Please enter a longitude value")
  } else if (is.na(lat) && !is.na(lon)) {
    stop("Please enter a latitude value")
  } else if (!is.na(lat) && !is.na(lon)) { #search by lat and lon
    if (!is.numeric(lat) || !is.numeric(lon) || lat < -180 || lat > 180 || lon < -180 || lon > 180) stop ("Invalid latitude/longitude value")
    url <- paste0(baseurl, lat, ",", lon, ".json")
    
  } else if (length(zipcode) > 1 || (length(zipcode) == 1 && !is.na(zipcode))) { #search by zipcode
    if (length(zipcode) > 1) stop("Please enter a single zipcode")
    if (!is.numeric(zipcode) || zipcode < 10000 || zipcode > 99999) stop("Please enter a valid zipcode")
    url <- paste0(baseurl, zipcode, ".json")
    
  } else { #search by city
    if (length(city) > 1) stop("Please enter a single city!")
    if (is.na(city) || !is.character(city) || city == "") stop("Invalid city name!")
    if (length(state) == 1 && length(country) == 1 && is.na(state) && is.na(country)) stop("Please enter a state or a country!")
    if (length(state) > 1 || (length(state) == 1 && !is.na(state))) { #by state, within USA
      if (length(state) > 1) stop("Please enter a single state")
      if (!is.character(state) || !state %in% state.abb) stop("Invalid state name!")
      url <- paste0(baseurl, state, "/", paste(strsplit(city, "\\s+")[[1]], collapse="_"), ".json")
    } else if (length(country) > 1 || (length(country) == 1 && !is.na(country))) { #by country, outside USA
      if (length(country) > 1) stop("Please enter a single country")
      if (!is.character(country)) stop("Invalid country name!")
      url <- paste0(baseurl, paste(strsplit(country, "\\s+")[[1]], collapse="_"), "/", paste(strsplit(city, "\\s+")[[1]], collapse="_"), ".json")
    }
    
  }
  
  r <- httr::GET(url)
  d <- jsonlite::fromJSON(rawToChar(r$content))
  if (!"location" %in% names(d) || class(d$location$nearby_weather_stations$pws$station)!="data.frame") stop("No matched PWS found based on your search!")
  
  if (d$location$country_name == "USA") {
    return (new("PWS", city=d$location$city, state=d$location$state, lat=as.numeric(d$location$lat), lon=as.numeric(d$location$lon), data=subset(d$location$nearby_weather_stations$pws$station, distance_km <= distance)))
  } else {
    return (new("PWS", city=d$location$city, country=d$location$country_name, lat=as.numeric(d$location$lat), lon=as.numeric(d$location$lon), data=subset(d$location$nearby_weather_stations$pws$station, distance_km <= distance)))
  }
}
