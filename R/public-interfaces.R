#' Make members public
#' 
#' @param x a default value
#' @param ... function definition
#' 
#' @usage publicFunction(...)
#' 
#' @rdname publicInterface
#' @export publicFunction
publicFunction <- setClass("publicFunction", contains = "function")

#' @export
#' @rdname publicInterface
publicValue <- function(x = NULL) {
  force(x)
  publicFunction(function(value) {
    if(missing(value)) {
      return(x)
    } else {
      x <<- value
      invisible(x)
    }
  })
}
