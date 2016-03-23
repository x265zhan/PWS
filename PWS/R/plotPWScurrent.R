## This File is Written by m1gao

#' plot the current weather condition based on the object of class \code{\link{PWS-class}}
#' 
#' \code{plotPWS} plots the current weather conditions for original search location and the nearby PWS's that are contained in an object of class \code{\link{PWS-class}}. It gets the weather condition by API calls from the website \url{http://www.wunderground.com}. The labels on the graph show temperatures in Fahrenheit scale. Users can choose to plot either over google map or as plain points.
#' 
#' @param object an object of class \code{\link{PWS-class}}
#' @param googleMap logical variable that indicates whether google map should be used for the plotting
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON 
#' @import ggplot2
#' @import ggmap
#' @import ggrepel
#' @import methods
#' @name plotPWScurrent
#' @rdname plotPWScurrent
#' @usage NULL
#' @exportMethod plotPWScurrent
#' @seealso \code{\link{PWS-class}}
#' @keywords methods
#' @keywords graphics
#' @note Please use this function with care since there are limits to the number of API calls per minute and per day from the website \url{http://www.wunderground.com}.
#' @examples
#' data(offlineExample)
#' offlineExample
#' plotPWScurrent(offlineExample, googleMap = FALSE) #plot without getting online google map
#' plotPWScurrent(offlineExample, googleMap = TRUE) #plot using online google map
#' 
#' 

utils::globalVariables(c("lon", "lat", "label", "weather", "temp"))

setGeneric("plotPWScurrent", function(object, googleMap = TRUE) standardGeneric("plotPWScurrent"))

#' @describeIn plotPWScurrent plot the current weather condition based on an object of class "PWS"
setMethod("plotPWScurrent", "PWS",
          function(object, googleMap = TRUE) {
            
            getCondition <- function(id) { #get current weather condition for an individual PWS station
              key <- .getAPI() #generates an api key
              baseurl <- paste0("http://api.wunderground.com/api/", key, "/conditions/q/PWS:")
              url <- paste0(baseurl, id, ".json")
              r <- httr::GET(url)
              d <- jsonlite::fromJSON(rawToChar(r$content))
              if ("error" %in% names(d$response)) warning(sprintf("Current weather condition for PWS id %s is not found", id))
              list(id = id, temp = d$current_observation$temp_f, weather = d$current_observation$weather)
            }
            
            # merge obtained PWS weather data with PWS location data
            data = lapply(object@data$id, getCondition)
            data = do.call(rbind, lapply(data, data.frame, stringsAsFactors = FALSE))
            data2 <- rbind(data.frame(city = "Origin Location", lon = object@lon, lat = object@lat, id = NA), object@data[,c("city", "lon", "lat", "id")])
            data2$label <- ifelse(data2$city == "Origin Location", "Origin", "PWS")
            myshape <- c("Origin" = 17, "PWS" = 16)
            data = merge(data, data2, by = "id", all = TRUE)
            
            if (googleMap) { #plot on google map
              map <- ggmap::get_googlemap(center = c(lon = object@lon, lat = object@lat), zoom = ggmap::calc_zoom(lon, lat, data) - 2, maptype="terrain", style = 'feature:all|element:labels|visibility:off')
              ggmap::ggmap(map, extent = "device") + ggplot2::geom_point(data = data, ggplot2::aes(x = lon, y = lat, shape = label, color = weather), size = 5, alpha = 0.6) + ggplot2::scale_shape_manual(values = myshape) + ggrepel::geom_text_repel(data = data, ggplot2::aes(lon, lat, label = temp), box.padding = ggplot2::unit(0.6, 'lines'), point.padding = ggplot2::unit(0.8, 'lines'), na.rm = TRUE) + ggplot2::ggtitle(paste0("Current weather condition of nearby PWS\n", Sys.time(),"\n(Temperatures in Fahrenheit scale)"))
              
            } else { #plot plain points
              ggplot2::ggplot(data) + ggplot2::geom_point(ggplot2::aes(lon, lat, shape = label, color = weather), size = 3) + ggplot2::scale_shape_manual(values = myshape) + ggrepel::geom_text_repel(ggplot2::aes(lon, lat, label = temp, color = weather), box.padding = ggplot2::unit(0.6, 'lines'), point.padding = ggplot2::unit(0.8, 'lines'), na.rm = TRUE) + ggplot2::ggtitle(paste0("Current weather condition of nearby PWS\n", Sys.time(),"\n(Temperatures in Fahrenheit scale)"))
            }
          })