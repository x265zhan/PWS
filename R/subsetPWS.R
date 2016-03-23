## This File is Written by m1gao

#' subset the \code{data} slot in an object of class \code{\link{PWS-class}}
#' 
#' \code{subsetPWS} extracts subtables of the \code{data} slot in an object of class \code{\link{PWS-class}} by distance (with respect to the original search location). It keeps the PWS which are within the specified distance of original search location.
#' 
#' @param object an object of class \code{\link{PWS-class}}
#' @param distance a positive numeric that specifies the distance (in kilometers) to subset the \code{data} slot
#' @return an object of class \code{\link{PWS-class}}
#' @name subsetPWS
#' @rdname subsetPWS
#' @import methods
#' @usage NULL
#' @exportMethod subsetPWS
#' @seealso \code{\link{PWS-class}}
#' @keywords methods
#' @examples
#' data(offlineExample)
#' offlineExample
#' b = subsetPWS(offlineExample, 1)
#' 
utils::globalVariables(c("distance_km"))

setGeneric("subsetPWS", function(object, distance) standardGeneric("subsetPWS"))


#' @describeIn subsetPWS subsets an object of class "PWS"
setMethod("subsetPWS", "PWS",
          function(object, distance) {
            if (length(distance) > 1) stop("Please enter a single distance value!")
            if (!is.numeric(distance) || distance < 0) stop("Invalid distance value!")
            object@data <- subset(object@data, distance_km <= distance)
            validObject(object)
            object
          })