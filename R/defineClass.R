#' Define a new class
#' 
#' Use \code{\link{Class}} instead of this function. \code{defineClass} has side effects. The constructor is the return value of \code{defineClass}. Use \code{public} to make things public; everything else will be private.
#'  
#' @param name character name of the class
#' @param expr expression
#' @param contains character name of class from which to inherit
#' 
#' @details 
#' \code{defineClass} creates a S4-Class which can be used for standard S4 method dispatch. It will also set the method 'initialize' which need not to be changed. If you want to have some operations carried out on initialization use a function definition named \code{init} as part of \code{expr}. The return value from \code{defineClass} is the constructor function. It has the argument \code{...} which will be passed to \code{init}.
#' 
#' All classes defined with \code{defineClass} inherit from class "aoos" which is a S4-class containing an environment. In that environment \code{expr} is evaluated; for inheritance, all \code{expr} from all parents will be evaluated first.
#' 
#' Everything in \code{expr} will be part of the new class definition. If you want to make objects public use \code{public}. If \code{x} in a call to \code{public} is a function it will be a public member function (method). For any other class the return value of \code{public} is a get and set method. If called without argument it will get the value, if called with argument it will set the value. You can define a validity function which will be called whenever the set method is called. Objects which inherit from class \code{environment} can be accessed directly, i.e. not via get/set methods.
#' 
#' @rdname defineClass
#' @export
#' @examples
#' test <- defineClass("test", {
#'   x <- public("Working ...")
#'   y <- 0
#'   doSomething <- public(function() {
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
#'
#' # Example for reference classes as field
#' MoreTesting <- defineClass("MoreTesting", {
#'   refObj <- public(test())
#' })
#' instance <- MoreTesting()
#' instance$refObj$x()
defineClass <- function(name, expr, contains = NULL) {

  mc <- processMarkup(match.call())
  parentEnv <- parent.frame()
  
  getMember <- function() {
    e <- setEnvironment(contains, parentEnv) # name e is needed in eval(mc)
    eval(mc)
    arrangeEnvironment(e)
  }
  
  const <- function(...) {
    object <- do.call("new", c(list(Class = name)))
    init(object, ...)
  }
  
  setClass(name, where = parentEnv, 
           contains = if(is.null(contains)) "aoos" else contains)
  
  setMethod("initialize", name,
            function(.Object, ...) {
              .Object@.xData <- getMember()
              parent.env(.Object)$self <- .Object
            }, where = parentEnv)
  
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
    object <- new(contains)
    parent.env(object)
  }
}

arrangeEnvironment <- function(e) {
  allMember <- as.list(e, all.names = TRUE)
  publicMemberInd <- sapply(allMember, function(obj) inherits(obj, "public"))
  publicMember <- lapply(allMember[publicMemberInd], getPublicRepresentation)
  f <- list2env(publicMember)
  parent.env(f) <- e
  f
}

init <- function(object, ...) {
  
  if(exists("init", envir = parent.env(object), inherits = FALSE)) {
    parent.env(object)$init(...)
  }
  
  object
}