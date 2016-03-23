## This File is Written by x265zhan

#' visualize properties of the \code{data} slot in an object of class \code{\link{PWSweather-class}}
#' 
#' \code{plotPWSweather} plots specific \code{variable} of each station in \code{data} slot in an object of class \code{\link{PWSweather-class}}
#'  
#' @param object an object of class \code{\link{PWSweather-class}}
#' @param variable a single character string that represent the target of the plot, must be one of "Temperature", "Humidity", "Windspeed" or "Pressure"
#' @import ggplot2
#' @importFrom reshape2 melt
#' @import methods
#' @return a plot shows the attribute define by \code{variable} vs time of each station
#' @name plotPWSweather
#' @rdname plotPWSweather
#' @usage NULL
#' @exportMethod plotPWSweather
#' @seealso \code{\link{PWSweather-class}}
#' @keywords methods
#' @keywords graphics
#' @examples
#' data(offlineWeather)
#' plotPWSweather(offlineWeather, 'Temperature')
#' plotPWSweather(offlineWeather, 'Humidity')
#' 
utils::globalVariables(c("value", "neighborhood"))

setGeneric("plotPWSweather", function(object, variable) standardGeneric("plotPWSweather"))

#' @describeIn imputePWS imputes an object of class "PWSweather" by a given variable
setMethod("plotPWSweather", "PWSweather",
          function(object, variable) {
            validVar <- c('Temperature', 'Humidity', 'Windspeed', 'Pressure')
            variable <- unique(variable)
            if (!all(variable %in% validVar) || length(variable) != 1) {
              stop("Input variable can only be 'Temperature', 'Humidity', 'Windspeed or 'Pressure' with length 1")
            }
            mapBook <- list('Temperature' = 'tempi','Humidity' = 'hum', 'Windspeed' = 'wspdi', 'Pressure' = 'pressurei')
            plotFun <- function(v) {
              dat <- reshape2::melt(imputePWS(object, mapBook[[v]]), id.vars = 'time')
              dat$neighborhood <- as.factor(unlist(lapply(strsplit(as.character(dat$variable), "-"), function (x) x[2])))
              ggplot2::ggplot(data = dat, ggplot2::aes(x = time, y = value, colour = neighborhood)) +
                ggplot2::geom_line() + 
                ggplot2::coord_cartesian(xlim = c(min(dat$time), max(dat$time))) +
                ggplot2::theme_classic(base_size = 16) +
                ggplot2::theme(legend.position = "bottom", legend.text = ggplot2::element_text(size = 8)) +
                ggplot2::labs(x = "Time", y = v, title = sprintf("%s: %s of Neighborhood", Sys.Date(), v))
            }
            plotFun(variable)
          })