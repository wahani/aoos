#' Helpers for environments
#' 
#' Functions to help working with environments.
#' 
#' @details \code{envCopy} tries to copy all objects in a given environment into the environment 'to'. Returns the names of copied objects.
#' 
#' @param from environment
#' @param to environment
#' @param x environment
#' @param with environment
#' 
#' @seealso \link{retList} where these are relevant.
#' @rdname envHelper
#' @export
envCopy <- function(from, to) {
  eNames <- ls(from, all.names = TRUE)
  eNames <- eNames[!(eNames == "...")]
  for (n in eNames) {
      assign(n, get(n, envir = from, inherits = FALSE), envir = to)
  }
  eNames
}

#' @details \code{envMerge} will merge x and with. Merge will copy all objects from x to with. Prior to that, the environment of functions are changed to be with iff functions in x have environment x; else the environment of functions are preserved.
#' @rdname envHelper
#' @export
envMerge <- function(x, with) {
  
  setEnvironmentOfLocalFuns <- function(x, with) {
    functionNames <- funNames(x)
    for (n in functionNames) {
      fun <- get(n, envir = x)
      if (identical(environment(fun), x)) {
        environment(fun) <- with
        assign(n, fun, envir = x)
      }
    }
  }
  
  setEnvironmentOfLocalFuns(x, with)
  envCopy(x, with)
  with
  
}



