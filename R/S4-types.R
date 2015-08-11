#' Types
#' 
#' This function can be used to define new S4-classes which are called Type - although they are simply S4-classes. They have an initialize method and in the introduced syntax init-method and S4-class definition build a unit, hence a type. This simply captures a typical \code{setClass} then \code{setMethod("initialize", ...)} pattern where often some redundancy is introduced. The function has side effects due to calling \code{setClass}, \code{setMethod} and assigning the constructor function to the types name. Furthermore a call to \link{globalVariables} for the name of the type and the slots is performed.
#' 
#' @param lhs an expression of the form: \cr\code{[<parent-name>:]<type-name>([<slots>])}
#' \cr - <parent-name> optional, the name of the S4-class/type to inherit from.
#' \cr - <type-name> the name for the new S4-class/type and constructor function.
#' \cr - <slots> optional, \code{name = value} expressions. They will be used to construct the prototype. The values will also be used to infer the class of the slots. If no value is supplied, \code{ANY} is assumed.
#' @param rhs the body of the initialize method as expression. It will be called with \code{.Object} and \code{...} as arguments. \code{.Object} should be the return value and it is the instance on which assertions can be formulated or whatever it is you want. Prior to the body (rhs) \code{.Object <- callNextMethod()} will be evaluated which enables proper initialization if your type and its inherited fields. See \link[methods]{initialize} for details.
#' 
#' @examples 
#' # This will create an S4-class named 'Test' with two slots; x = "numeric"
#' # and y = "list"; prototype: list(x = 1, y = list()); and an initialize
#' # method where some checks are performed.
#' 
#' Test(x = 1, y = list()) %type% {
#'   stopifnot(.Object@@x > 0)
#'   .Object
#' }
#'
#' @export
"%type%" <- function(lhs, rhs) {
  
  wrapInCurlyBraces <- function(x) {
    if (length(x) == 1) c("{", gsub("^\\{|\\}$", "", x), "}")
    else x
  }
  
  mc <- match.call()
  lhs <- deparse(mc$lhs)
  envir <- parent.frame()
  
  # Name of class and super:
  classes <- rev(splitTrim(deleteInParan(lhs), ":"))
  className <- classes[1]
  super <- if (is.na(classes[2])) character() else classes[2]
  
  # Init-method call:
  initCall <- "initialize(.Object = " %p0% className %p0% ", ...)"
  initBody <- deparse(mc$rhs)
  initBody <- wrapInCurlyBraces(initBody)
  initBody[1] <- initBody[1] %p0% "\n.Object <- callNextMethod()"
  initCall <- parse(text = initCall %p% "%m%" %p% paste(initBody, collapse = "\n"))
  
  # slots & prototype & constructor:
  slotsCall <- deleteBeforeParan(lhs) %>% deleteEnclosingParan %>% splitTrim(",")
  ind <- grepl("=", slotsCall)
  slotsCall[!ind] <- slotsCall[!ind] %p0% " = NULL"
  slotsCall <- "(" %p0% paste(slotsCall, collapse = ", ") %p0% ")"
  proto <- eval(parse(text = "list" %p0% slotsCall), envir = parent.frame())
  slots <- vapply(proto, function(slot) if (is.null(slot)) "ANY" else class(slot)[1], character(1))
  slots <- slots[names(slots) != "..."]
  argsInNew <- if (length(proto) == 0) "" else (", " %p0%
    paste(names(proto) %p% "=" %p% names(proto), collapse = ", "))
  argsInConst <- sub("\\)$", if (length(proto) == 0) "...)" else ", ...)", slotsCall)
  constCall <- "function" %p0% argsInConst %p% 
    "new('" %p0% className %p0% "'" %p0% argsInNew %p0% ", ...)"
  const <- eval(parse(text = constCall), envir = envir)
  
  # class:
  setClass(className, contains = super, prototype = proto, slots = slots, where = parent.frame())
  
  # init-method
  eval(initCall, envir)
  
  # Return const as side effect and trick R CMD check:
  globalVariables(c(className, names(slots)), package = topenv(envir))
  assign(className, const, envir = envir)
  invisible(const)
  
}
