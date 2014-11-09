#' Class oom
#' 
#' This is an environment with some methods. Every class defined by \code{defineClass} will inherit from oom.
#' 
#' @import methods
#' @exportClass oom
#' @rdname oom
setClass("oom", contains = c("environment", "VIRTUAL"))

#' @rdname oom
#' @export
setMethod("show", signature = c(object = "oom"), 
          function(object) {
            cat("Approximated memory size:", 
                sum(envSize(parent.env(object))$Size), 
                "(Mib)\n")
            cat("public member:\n")
            lapply(ls(object), function(n) cat(" ", n, "\n"))
            #             print(env.profile(as.environment(object)))
          })

#' @rdname oom
#' @export
#' @param x object
#' @param name member name
setMethod("$", signature = c(x = "oom"),
          function(x, name) {
            if(exists(name, envir = x, inherits = FALSE)) {
              get(name, envir = x)
            } else {
              stop(paste(name, "is not a public member."))
            }
          })

#' @rdname oom
#' @export
#' @param value value to assign to. Will throw an error.
setMethod("$<-", signature = c(x = "oom"),
          function(x, name, value) {
            stop("If you want to add public fields use 'publicValue' in the class definition.")
          })

#' @rdname oom
#' @param object object
#' @param ... arguments passed to method (not used).
#' @export
setMethod("summary", signature = c(object = "oom"),
          function(object, ...) {
            envSize(parent.env(object))
          })

envSize <- function (env) {
  
  napply <- function(names, fn) sapply(names, function(x) fn(get(x, pos = env)))
  
  names <- ls(env, all.names = TRUE)
  
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size)
  
  obj.subsizes <- napply(names, function(x) {
    if(inherits(x, "publicFunction") && !identical(environment(x), env)) 
      envSize(environment(x))
  })
  
  obj.subsizes <- do.call(rbind, obj.subsizes)
  
  out <- rbind(data.frame(Type = obj.type, "Size.Mib" = round(obj.size / (1024^2), 1)), obj.subsizes)
  out[order(rownames(out), out$Type), ]
  
}