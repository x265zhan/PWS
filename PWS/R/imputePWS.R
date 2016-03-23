## This File is Written by x265zhan

#' register the \code{data} slot in an object of class \code{\link{PWSweather-class}} for some reported \code{variable} from individual stations
#' 
#' \code{imputePWS} registers the \code{data} slot in an object of class \code{\link{PWS-class}} for some reported \code{variable} by interpolating or otherwise approximating the variable for each station through linear approximation
#' 
#' @param object an object of class \code{\link{PWSweather-class}}
#' @param variable a single character string that target variable that needs to be imputed, must be one of "tempi" (Temperature), "hum" (Humidity), "wspdi" (Windspeed) or "pressurei" (Pressure)
#' @return a data frame contain columns of imputed value of each station, and a column of timestamp
#' @import methods
#' @name imputePWS
#' @rdname imputePWS
#' @usage NULL
#' @exportMethod imputePWS
#' @seealso \code{\link{PWSweather-class}}
#' @keywords methods
#' @examples
#' data(offlineWeather)
#' d = imputePWS(offlineWeather, 'tempi')
#' 
setGeneric("imputePWS", function(object, variable) standardGeneric("imputePWS"))


#' @describeIn imputePWS imputes an object of class "PWSweather" by a given variable
setMethod("imputePWS", "PWSweather", 
          function(object, variable) {
            weatherData <- lapply(object@data, function(x) {approx(x[,variable])$y})
            table <- data.frame(matrix(unlist(weatherData), ncol = length(weatherData), byrow = FALSE))
            names(table) <- names(object@data)
            time <- seq(strptime(object@start, format = "%H:%M"), 
                        strptime(object@end, format = "%H:%M"), 
                        length.out = nrow(table))
            cbind(table, time)
          })