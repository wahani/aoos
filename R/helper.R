#' Helpers for environments
#' 
#' Functions to help working with environments.
#' 
#' @details \code{envCopy} tries to copy all objects in a given environment into the environment 'to'. Returns the names of copied objects. Objects in 'to' are not replaced.
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
  for (n in eNames) {
    if (!exists(n, to, inherits = FALSE))
      assign(n, get(n, envir = from, inherits = FALSE), envir = to)
  }
  eNames
}

#' @details \code{envMerge} will merge x and with. Merge will copy all objects from with to x. Prior to that, the environment of functions are changed to be x.
#' @rdname envHelper
#' @export
envMerge <- function(x, with) {
  namesToCopy <- ls(with, all.names = TRUE)
  funs <- envGetFuns(namesToCopy, with)
  funs <- listKeepFuns(funs)
  funs <- setEnvironmentOfFuns(funs, x)
  envCopy(list2env(funs), x)
  envCopy(with, x)
}

envGetFuns <- function(names, from) mget(names, from, "function", vector("list", length(names)), FALSE)

listKeepFuns <- function(funList) funList[unlist(lapply(funList, Negate(is.null)))]

setEnvironmentOfFuns <- function(funList, e) lapply(funList, `environment<-`, value = e)

