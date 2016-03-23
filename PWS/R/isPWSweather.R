## This File is Written by x265zhan

#' check whether an object is a "PWSweather" class
#' 
#' @param object an object could be any class
#' @return a binary response is either TRUE or FALSE
#' @name isPWSweather
#' @rdname isPWSweather
#' @export
#' @seealso \code{\link{PWSweather-class}}
#' @examples
#' data(offlineWeather)
#' isPWSweather(offlineWeather)
isPWSweather <- function(object) {
  return ("PWSweather" %in% class(object))
}