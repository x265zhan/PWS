## This File is Written by x265zhan

#' get an object of class "PWSweather"
#' 
#' Gets the up to date weather information about nearby PWS (personal weather station) from \url{http://www.wunderground.com} based on a PWS object, a starting time and an ending time 
#' 
#' The function retrieves weather information about all stations listed in \code{pws} and subset the data based on the time interval which is specified by \code{start} and \code{end}     
#' 
#' @param pws a PWS object that contains the information about the original search location and the nearby PWS 
#' @param start a single character string in HH:MM or H:MM form that shows the starting time of the time interval
#' @param end a single character string in HH:MM or H:MM form that shows the ending time of the time interval; must be greater than starting time
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON 
#' @return An S4 object of class \code{\link{PWSweather-class}} contains the information about the weather condition of some PWS (personal weather stations) on a given time interval
#' @note please use this function with care since there are limits to the number of API calls per minute and per day 
#' @name getPWSweather
#' @rdname getPWSweather
#' @export
#' @seealso \code{\link{PWSweather-class}}
#' @examples
#' data(offlineExample)
#' a = getPWSweather(offlineExample, "3:00", "17:00")
#' validObject(a)
#'
getPWSweather <- function(pws, start, end){
  if (! "PWS" %in% class(pws)) {# check input
    stop("Input should be a valid PWS object")
  }
  if (length(start) > 1) {
    stop("start time is not a single character string")
  }
  if (length(end) > 1) {
    stop("end time is not a single character string")
  }
  starttime <- strptime(start, format = "%H:%M")
  endtime <-strptime(end, format = "%H:%M")
  if (length(start) == 1 && is.na(starttime)) {
    stop("start time should be valid time in the form of HH:MM")
  }
  if (length(end) == 1 && is.na(endtime)) {
    stop("end time should be valid time in the form of HH:MM")
  }
  data <- list()
  ids <- pws@data$id
  neighbors <- pws@data$neighborhood
  index <- 1
  date <- format(Sys.Date(), "%Y%m%d")
  baseurl <- "http://api.wunderground.com/api/%s/history_%s/q/pws:%s.json"
  for (id in ids) {
    api <- .getAPI() #generates an api key
    url <- sprintf(baseurl,api,date,id)
    r <- httr::GET(url)
    d <- jsonlite::fromJSON(rawToChar(r$content))
    table <- d$history$observations
    if (is.data.frame(table) && nrow(table) > 1) { # generate the table only if it has at least 2 rows
      table$timestamp <- strptime(paste(table$date$hour, table$date$min, sep = ":"), format = "%H:%M")
      data[[paste(id, paste(index, neighbors[index], sep = ' '), sep = '-')]] <- subset(table, timestamp >= starttime & timestamp <= endtime)
    }
    index <- index + 1
  }
  return (new("PWSweather", data = data, start = start, end = end))
}