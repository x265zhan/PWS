## This File is Written by x265zhan

#' check whether an object is a "PWS" class
#' 
#' @param object an object could be any class
#' @return a binary response is either TRUE or FALSE
#' @name isPWS
#' @rdname isPWS
#' @export
#' @seealso \code{\link{PWS-class}}
#' @examples
#' data(offlineExample)
#' isPWS(offlineExample)
isPWS <- function(object) {
  return ("PWS" %in% class(object))
}