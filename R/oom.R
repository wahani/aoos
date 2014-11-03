#' Class oom
#' 
#' This is an environment with some methods for access restriction.
#' 
#' @import methods
#' @exportClass oom
#' @rdname oom
setClass("oom", contains = "environment")

setMethod("show", signature = c(object = "oom"), 
          function(object) {
            cat("public member:\n")
            lapply(ls(object), function(n) cat(" ", n, "\n"))
            #             print(env.profile(as.environment(object)))
          })

#' @rdname oom
#' @param x object
#' @param name member name
setMethod("$", signature = c(x = "oom"),
          function(x, name) {
            if(exists(name, envir = x, inherits = FALSE)) {
              get(name, envir = x)
            } else {
              stop(paste(name, "is not public."))
            }
          })

#' @rdname oom
#' @param value value to assign to (don't do this)
setMethod("$<-", signature = c(x = "oom"),
          function(x, name, value) {
            stop("If you want to add public fields use 'publicValue' in the class definition.")
          })
