#' Expose environment
#' 
#' Tries to copy all objects in a given environment into the environment 'to'. Returns the names of copied objects.
#' 
#' @param from environment
#' @param to environment
#' 
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

#' @rdname envHelper
#' @export
envMerge <- function(from, to) {
  namesToCopy <- ls(from, all.names = TRUE)
  funs <- envGetFuns(namesToCopy, from)
  funs <- listKeepFuns(funs)
  funs <- setEnvironmentOfFuns(funs, to)
  envCopy(list2env(funs), to)
  envCopy(from, to)
}

envGetFuns <- function(names, from) mget(names, from, "function", vector("list", length(names)), FALSE)

listKeepFuns <- function(funList) funList[unlist(lapply(funList, Negate(is.null)))]

setEnvironmentOfFuns <- function(funList, e) lapply(funList, `environment<-`, value = e)

