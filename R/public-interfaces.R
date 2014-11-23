setClass("publicFunction", contains = "function")
setClass("publicValue", contains = "publicFunction")

#' Constructors for public members
#' 
#' These functions are used internally. You should not rely on them. Use \code{\link{public}} instead.
#' 
#' @param x a default value
#' @param validity an optional validity function for the set method. Returns TRUE or FALSE.
#' @param fun function definition
#' 
#' @rdname publicInterface
#' @export publicFunction
publicFunction <- function(fun) {
  new("publicFunction", .Data = fun)
}

#' @export
#' @rdname publicInterface
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

#' @param x an object made public
#' @param validity function to check the validity of an object
#' 
#' @rdname defineClass
#' @export
setGeneric("public", function(x = NULL, validity = function(x) TRUE) {
  publicValue(x, validity)
})

#' @rdname defineClass
#' @export
setMethod("public", c(x = "function"), function(x, validity) {
  publicFunction(x)
})
