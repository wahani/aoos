#' Return your current environment as list
#' 
#' This functions can be used to construct a list with class attribute and merged with another list called super.
#' @param class character giving the class name
#' @param exports character with the names to include
#' @param super object returned by this function which should be extended
#' 
#' @seealso \link{ls}
#' @rdname retList
#' @export
retList <- function(class = NULL, exports = NULL, super = list()) {
  envir <- parent.frame()
  superClasses <- if(is.null(super)) "list" else class(super)
  child <- if(is.null(exports)) as.list(envir) else mget(exports, envir)
  super[names(child)] <- child
  class(super) <- c(class, superClasses)
  super
}

#' \code{funNames} returns the names of functions in the environment from which it is called.
#' 
#' @rdname retList
#' @export
funNames <- function() {
  envir <- parent.frame()
  funInd <- unlist(eapply(envir, is.function))
  names(funInd)[funInd]
}