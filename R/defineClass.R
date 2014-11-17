#' Define a new class
#' 
#' \code{defineClass} has side effects. It creates a S4-Class which can be used for writing S4-methods. The constructor is the return value of \code{defineClass} and need to be assigned to a varaible named \code{name}. Use \code{publicFunction} and \code{publicValue} to make things public; everything else will be private.
#'  
#' @param name character name of the class
#' @param expr expression
#' @param contains character name of class from which to inherit
#' 
#' @details 
#' All classes inherit from class "oom" which is a S4-class containing an environment. In that environment \code{expr} is evaluated; for inheritance, all \code{expr} from all parents will be evaluated first. The constructor function is the return value of \code{defineClass} and needs to be named \code{name}. 
#' 
#' Everything in \code{expr} will be part of the new class definition. If you want to make objects public use \code{publicFunction} and \code{publicValue}. 
#' 
#' \code{publicValue} will create a function, if called without argument it will get the value, if called with argument it will set the value. You can set a validity function.
#' 
#' @rdname defineClass
#' @export
#' @examples
#' test <- defineClass("test", {
#'   x <- publicValue("Working ...")
#'   y <- 0
#'   doSomething <- publicFunction(function() {
#'     self$y <- y + 1
#'     cat(x(), "\n")
#'     invisible(self)
#'   })
#' })
#' instance <- test()
#' \dontrun{
#' instance$y # error
#' }
#' instance$doSomething()$doSomething()
#' instance$x()
#' instance$x(2)
#' instance$x()
defineClass <- function(name, expr, contains = NULL) {

  mc <- processMarkup(match.call())
  parentEnv <- parent.frame()
  
  getMember <- function() {
    e <- setEnvironment(contains, parentEnv) # name e is needed in eval(mc)
    eval(mc)
    arrangeEnvironment(e)
  }
  
  const <- function(...) {
    object <- do.call("new", c(list(Class = name), .xData = getMember()))
    init(object, ...)
  }
  
  setClass(name, where = parentEnv, 
           contains = if(is.null(contains)) "oom" else contains)
  
  invisible(const)
}

processMarkup <- function(mc) {
  mc[[1]] <- quote(eval)
  mc$envir <- quote(e)
  expr <- mc$expr
  mc$expr <- substitute(expression(expr))
  mc$name <- NULL
  mc$contains <- NULL
  mc
}

setEnvironment <- function(contains, parentEnv) {
  if(is.null(contains)) {
    new.env(parent = parentEnv)
  } else {
    object <- get(contains, envir = parentEnv, inherits = TRUE)()
    parent.env(object)
  }
}

arrangeEnvironment <- function(e) {
  allMember <- as.list(e, all.names = TRUE)
  publicMemberInd <- sapply(allMember, function(obj) inherits(obj, "publicFunction"))
  publicMember <- allMember[publicMemberInd]
    
  f <- as.environment(publicMember)
  parent.env(f) <- e
#   e$self <- e
  f
}

init <- function(object, ...) {
  
  parent.env(object)$self <- object
  
  if(length(list(...))) {
    if(exists("init", envir = parent.env(object), inherits = FALSE)) {
      parent.env(object)$init(...)
    } else {
      stop("Found no function 'init'.")
    }
  }
  
  object
}