setClass("publicFunction", contains = "function")
setClass("publicValue", contains = "publicFunction")

#' @param x a default value
#' @param validity an optional validity function for the set method. Returns TRUE or FALSE.
#' @param fun function definition
#' 
#' @rdname defineClass
#' @export publicFunction
publicFunction <- function(fun) {
  new("publicFunction", .Data = fun)
}

#' @export
#' @rdname defineClass
publicValue <- function(x = NULL, validity = function(x) TRUE) {
  force(x); force(validity)
  new("publicValue", .Data = function(value) {
    if(missing(value)) {
      return(x)
    } else {
      if(validity(value)) {
        x <<- value
        invisible(x)
      } else {
        stop("Invalid value!")
      }
    }
  })
}
