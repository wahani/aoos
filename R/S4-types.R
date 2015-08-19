#' Types
#' 
#' This function can be used to define new S4-classes which are called Type. They have an initialize method and in the introduced syntax init-method and S4-class definition build a unit, hence a type. This simply captures a typical \code{setClass} then \code{setMethod("initialize", ...)} pattern where often some redundancy is introduced. The function has side effects due to calling \code{setClass}, \code{setMethod} and assigning the constructor function to the types name.
#' 
#' @param lhs an expression of the form: \cr\code{[<parent-name>:]<type-name>([<slots>])}
#' \cr - <parent-name> optional, the name of the S4-class/type to inherit from.
#' \cr - <type-name> the name for the new type and constructor function.
#' \cr - <slots> optional, \code{name = value} expressions. They will be used to construct the prototype. The values will also be used to infer the class of the slots. If no value is supplied, \code{ANY} is assumed.
#' @param rhs the body of the initialize method as expression. It will be called with \code{.Object} and \code{...} as arguments. \code{.Object} should be the return value. With \code{.Object} there is an instance of the type on which assertions can be formulated. Prior to the body (rhs) \code{.Object <- callNextMethod()} will be evaluated which enables proper initialization if your type and its inherited fields. See \link[methods]{initialize} for details.
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
#' # This will create an S4-class named 'Numeric' with a slot and some tests.
#' 
#' numeric : Numeric(metaInfo = character()) %type% {
#'   stopifnot(length(.Object) > 0)
#'   stopifnot(all(.Object > 0))
#'   .Object
#' }
#' 
#' @export
"%type%" <- function(lhs, rhs) {
  
  wrapInCurlyBraces <- function(x) {
    if (length(x) == 1) c("{", gsub("^\\{|\\}$", "", x), "}")
    else x
  }
  
  protoIsGood <- function(proto) 
    proto@dataPart || !identical(proto@slots, character())
  
  evalInParent <- function(text) eval(parse(text = text), envir = envir)
  
  mc <- match.call()
  lhs <- deparse(mc$lhs)
  envir <- parent.frame()
  
  # Name of class and super:
  classes <- deleteInParan(lhs) %>% splitTrim(":") %>% deleteQuotes %>% rev
  className <- classes[1]
  super <- if (is.na(classes[2])) character() else classes[2:length(classes)]
  
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
  allArgs <- "(" %p0% paste(slotsCall, collapse = ", ") %p0% ")"
  protoArgs <- sub(".Data( )?\\=", "", allArgs)
  constArgs <- "(" %p0% paste(slotsCall[!grepl("^.Data( )?\\=|\\.\\.\\.", slotsCall)], collapse = ", ") %p0% ")"
  proto <- evalInParent("prototype" %p0% protoArgs)
  protoAsList <- evalInParent("list" %p0% constArgs)
  slots <- vapply(protoAsList, function(slot) if (is.null(slot)) "ANY" else class(slot)[1], character(1))
  argsInNew <- if (length(protoAsList) == 0) "" else (", " %p0%
    paste(names(protoAsList) %p% "=" %p% names(protoAsList), collapse = ", "))
  argsInConst <- sub("\\)$", if (length(protoAsList) == 0) "...)" else ", ...)", constArgs)
  constCall <- "function" %p0% argsInConst %p% 
    "new('" %p0% className %p0% "'" %p0% argsInNew %p0% ", ...)"
  const <- evalInParent(constCall)
  
  # class:
  argList <- list()
  argList$Class <- className
  argList$contains <- super
  argList$prototype <- if (protoIsGood(proto)) proto else NULL
  argList$slots <- slots
  argList$where <- envir
  
  do.call(setClass, argList)
  
  # init-method
  eval(initCall, envir)
  
  # Return const as side effect and trick R CMD check:
  globalVariables(c(className, names(slots)), package = topenv(envir))
  assign(className, const, envir = envir)
  invisible(getClass(className, where = envir))
  
}
