## This File is Written by m1gao

#' plot the geographical location of the object of class \code{\link{PWS-class}}
#' 
#' \code{plotPWS} plots the original search location and the nearby PWS's that are contained in an object of class \code{\link{PWS-class}}. It gives a visualization of the relative locations of origin and PWS. Users can choose to plot either over google map or as plain points.
#' 
#' @param object an object of class \code{\link{PWS-class}}
#' @param googleMap logical variable that indicates whether google map should be used for the plotting
#' @import ggplot2
#' @import methods
#' @import ggmap
#' @import ggrepel
#' @name plotPWS
#' @rdname plotPWS
#' @usage NULL
#' @exportMethod plotPWS
#' @seealso \code{\link{PWS-class}}
#' @keywords methods
#' @keywords graphics
#' @examples
#' data(offlineExample)
#' offlineExample
#' plotPWS(offlineExample, googleMap = FALSE) #plot without getting online google map
#' plotPWS(offlineExample, googleMap = TRUE) #plot using online google map
#' 

utils::globalVariables(c("lon", "lat", "label", "city", "neighborhood"))

setGeneric("plotPWS", function(object, googleMap = TRUE) standardGeneric("plotPWS"))

#' @describeIn plotPWS plot the geographical location based on an object of class "PWS"
setMethod("plotPWS", "PWS",
          function(object, googleMap = TRUE) {
            data <- rbind(data.frame(neighborhood = "Origin Location", city = "Origin Location", lon = object@lon, lat = object@lat), object@data[,c("neighborhood", "city", "lon", "lat")])
            data$label <- ifelse(data$city == "Origin Location", "Origin", "PWS")
            myshape <- c("Origin" = 17, "PWS" = 16)
            
            if (googleMap) { #plot on google map
              map <- ggmap::get_googlemap(center = c(lon = object@lon, lat = object@lat), zoom = ggmap::calc_zoom(lon, lat, data) - 2, maptype="terrain", style = 'feature:all|element:labels|visibility:off')
              ggmap::ggmap(map, extent = "device") + ggplot2::geom_point(data = data, ggplot2::aes(x = lon, y = lat, shape = label, color = city), size = 5, alpha = 0.6) + ggplot2::scale_shape_manual(values = myshape) + ggrepel::geom_text_repel(data = data, ggplot2::aes(lon, lat, label = neighborhood), box.padding = ggplot2::unit(0.8, 'lines'), point.padding = ggplot2::unit(0.8, 'lines'), na.rm = TRUE) + ggplot2::ggtitle("Nearby PWS around original search location\n(text labels show neighborhood)")
              
            } else { #plot plain points
              ggplot2::ggplot(data) + ggplot2::geom_point(ggplot2::aes(lon, lat, shape = label, color = city), size = 2) + ggplot2::scale_shape_manual(values = myshape) + ggrepel::geom_text_repel(ggplot2::aes(lon, lat, label = neighborhood, color = city), box.padding = ggplot2::unit(0.8, 'lines'), point.padding = ggplot2::unit(0.8, 'lines'), na.rm = TRUE) + ggplot2::ggtitle("Nearby PWS around original search location\n(text labels show neighborhood)")
            }
          })
