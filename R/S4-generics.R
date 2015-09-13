#' Wrapper for writing S4 generics and methods
#' 
#' These are two wrappers around \code{setGeneric} and \code{setMethod}. A
#' relevant difference is that generics and methods are stored in the
#' environment in which  \code{\%g\%} and \code{\%m\%} are called and not in the
#' top-environment. Furthermore both functions have side effects in that they
#' will call \code{\link{globalVariables}} for the arguments and name of the
#' generic.
#' 
#' @param lhs see details
#' 
#' @param rhs the body as an expression.
#' 
#' @details 
#' The Syntax for the left hand side:
#'   \cr\code{[<valueClass>:]<genericName>(<argList>)}
#'   \cr - \code{valueClass} optional, is the class of the return value (see
#'   \link{setGeneric})
#'   \cr - \code{genericName} the name of the generic function
#'   \cr - \code{argList} are \code{name = value} or \code{name ~ type}
#'   expressions. Name-Value expressions are just like in a function definition.
#'   Name-Type expressions are used to define the signature of a method (see
#'   \link{setMethod}).
#' 
#' @examples 
#' # A new generic function and a method:
#' numeric : generic(x) %g% standardGeneric("generic") 
#' generic(x ~ numeric) %m% x
#' generic(1)
#' 
#' # Polymorphic methods in an object:
#' Object <- function() {
#'   numeric : generic(x) %g% standardGeneric("generic") 
#'   generic(x ~ numeric) %m% x
#'   retList("Object")
#' }
#' Object()$generic(1)
#' 
#' @export
#' @rdname S4generics
"%g%" <- function(lhs, rhs) {
  
  argList <- GenericExpressionTree(match.call(), parent.frame())
  
  # Fix for R CMD check:
  globalVariables(c(
    argList$name, 
    names(formals(argList$def))), 
    topenv(argList$where)
  )
  
  do.call("setGeneric", argList)
  invisible(getGeneric(argList$name, where = argList$where))
  
}

GenericExpressionTree <- function(.mc, where) {
  # .mc is a match.call() from %g% and where the parent frame
  # This function will construct a list of arguments for a call to setGeneric
  
  .exprTree <- ExpressionTree(.mc)
  name <- .exprTree$names[1]
  valueClass <- if (is.na(.exprTree$names[2])) character() else .exprTree$names[2]
  def <- makeFunDef(.exprTree$args, .exprTree$body, where)
  
  retList("GenericExpressionTree")
  
}

ExpressionTree <- function(.mc) {
  # Function to construct the central language object. Will be processed to be
  # used to construct argument lists for S4s set-functions (setClass,
  # setGeneric, setMethod)
  # The systax is as follows:
  # [name1 : ... : nameN-1 : ] nameN([<argList>]) %<fun>% expr
  # name1 to nameN will be the names of the generic / type and inheritance
  #   structure
  # <argList> the arguments. Can be usual argument expressions (argName =
  # defaultValue) or 'type expressions' (argName ~ typeName).
  # expr is the function body. 
  
  .seperate <- function(x, delim) {
    lArgs <- lapply(x, . %>% splitTrim(delim))
    args <- sapply(lArgs, . %>% .[2])
    names(args) <- argNames
    args
  }
  
  .lhs <- deparse(.mc$lhs) %>% paste(collapse = "") %>% sub("\\n", "", .)
  body <- deparse(.mc$rhs)
  names <- deleteInParan(.lhs) %>% splitTrim(":") %>% deleteQuotes %>% rev
  args <- deleteBeforeParan(.lhs) %>% deleteEnclosingParan %>% splitTrim(",") 
  argNames <- sapply(args, . %>% splitTrim("=|~") %>% .[1], USE.NAMES = FALSE)
  argDefaults <- args %>% .seperate("=")
  argClasses <- args %>% .seperate("~")
  
  retList("ExpressionTree")
  
}

makeFunDef <- function(args, body, envir) {
  # args and body are expected to be character which will then be parsed to
  # R-Code
  args <- if (is.character(args)) 
    "(" %p0% paste(args, collapse = ", ") %p0% ")" else 
      stop(args, "is a ", class(args))
  
  body <- if (is.character(body)) 
    paste(body, collapse = "\n") else 
      stop(body, "is a", class(body))
  
  defCall <- "function" %p0% args %p0% body
  eval(parse(text = defCall), envir = envir)
  
}

#' @export
#' @rdname S4generics
"%m%" <- function(lhs, rhs) {
  
  argList <- MethodExpressionTree(match.call(), parent.frame())
  
  # Fix for R CMD check:
  globalVariables(
    names(formals(argList$definition)), 
    topenv(argList$where)
  )
  
  do.call("setMethod", argList)
  invisible(getMethod(argList$f, argList$signature))
  
}

MethodExpressionTree <- function(.mc, where) {
  # Constructs the argument list for method::setMethod
  .exprTree <- ExpressionTree(.mc)
  f <- eval(parse(text = .exprTree$names[1]), envir = where)
  .genericArgNames <- names(formals(f)) %without% "..."
  
  signature <- .exprTree$argClasses[!is.na(.exprTree$argClasses)]
  
  .args <- ifelse(
    .exprTree$argNames %in% .genericArgNames, 
    .exprTree$argNames,
    .exprTree$args
  )
  
  definition <- makeFunDef(.args, .exprTree$body, where)
  
  retList("MethodExpressionTree")
  
}

# Helpers:
deleteQuotes <- . %>% sub("^[\"\']", "", .) %>% sub("[\"\']$", "", .)

deleteBeforeParan <- . %>% splitTrim("\\(") %>% { .[1] <- ""; . } %>% 
  paste0(collapse = "(")

deleteEnclosingParan <- . %>% sub("\\)$", "", .) %>% sub("^\\(", "", .)

deleteInParan <- . %>% gsub("\\(.*\\)", "", .)

splitTrim <- function(x, pattern) {
  strsplit(x, pattern) %>% unlist %>% trimws
}

"%p0%" <- function(lhs, rhs) paste0(lhs, rhs)

"%p%" <- function(lhs, rhs) paste(lhs, rhs)

"%without%" <- function(lhs, rhs) lhs[!(lhs %in% rhs)]
