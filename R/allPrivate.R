## This File is Written by m1gao

## Private functions used in the \code{\link{PWS-package}}

#' A function to get API keys from the website \url{http://www.wunderground.com}.
#' 
#' @param .getAPI returns an API key from the website \url{http://www.wunderground.com}
#' @rdname getAPI
#' @usage NULL
#' 
.getAPI <- function() {
  APIkeys <- c("a7df178cdf7cdf41", "4236877fc3cc871b", "16b5acac6e63503a", "0a8dfe94a25df310", "be33d7a5e3c42f6d", "745ffecab54fc04b", "6d1f4433161199bc", "103e184743b3b818", "7563ea7b8b4f3ea1", "b23f0c468ede9600", "9b5118de0a97bb22", "2c84d17295e3f5f0", "357709d147d0b620", "f18aced80399ef27", "de1ef94a6fccea2e", "200f59de420c4c21", "0454ce660e2b143d", "5b21c5021712aebe", "95f82561e26b443b", "cc3f4ef1e46dc211", "95040989218e5038")
  n <- sample.int(length(APIkeys), 1)
  return (APIkeys[n])
}