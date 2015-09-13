#' Types
#' 
#' This function can be used to define new S4-classes which are called Type.
#' They have an initialize method and in the introduced syntax init-method and
#' S4-class definition build a unit, hence a type. This simply captures a
#' typical \code{setClass} then \code{setMethod("initialize", ...)} pattern
#' where often some redundancy is introduced. The function has side effects due
#' to calling \code{setClass}, \code{setMethod} and assigning the constructor
#' function to the types name.
#' 
#' @param lhs an expression of the form:
#'   \cr\code{[<parent-name>:]<type-name>([<slots>])}
#' \cr - <parent-name> optional, the name of the S4-class/type to inherit from.
#' \cr - <type-name> the name for the new type and constructor function.
#' \cr - <slots> optional, \code{name = value} expressions. They will be used to
#' construct the prototype. The values will also be used to infer the class of
#' the slots. If no value is supplied, \code{ANY} is assumed.
#' @param rhs the body of the initialize method as expression. It will be called
#'   with \code{.Object} and \code{...} as arguments. \code{.Object} should be
#'   the return value. With \code{.Object} there is an instance of the type on
#'   which assertions can be formulated. Prior to the body (rhs) \code{.Object
#'   <- callNextMethod()} will be evaluated which enables proper initialization
#'   if your type and its inherited fields. See \link[methods]{initialize} for
#'   details.
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
  
  .exprTree <- ExpressionTree(match.call())
  .classExprTree <- ClassExpressionTree(.exprTree, parent.frame())
  .initExprTree <- InitMethodExpressionTree(.exprTree, parent.frame())
  .constExprTree <- ConstExpressionTree(.exprTree, parent.frame())
  
  do.call(setClass, .classExprTree)
  do.call(setMethod, .initExprTree)
  const <- do.call(makeFunDef, .constExprTree)
  assign(.exprTree$names[1], const, envir = parent.frame())
  
  globalVariables(c(
    .exprTree$names[1], 
    names(.classExprTree$slots)), 
    package = topenv(parent.frame())
  )
  
  invisible(getClass(.exprTree$names[1], where = parent.frame()))
  
}

ClassExpressionTree <- function(.exprTree, where) {
 
  .makeExpression <- function(funName, args) {
    parse(text = paste0(
      funName, "(", paste(args, collapse = ", "), ")"
    ))
  }
  
  .protoIsGood <- function(proto) 
    proto@dataPart || !identical(proto@slots, character())
  
  .localEval <- function(expr) eval(expr, envir = where)
  
  .getExplicitClass <- function(.exprTree) {
    slots <- .exprTree$argClasses
    slots[is.na(slots)] <- "ANY"
    slots <- slots[!(names(slots) == ".Data")]
    slots
  }
  
  .getPrototype <- function(.exprTree) {
    args <- .exprTree$argDefaults[!is.na(.exprTree$argDefaults)]
    args <- paste(names(args), args, sep = "=")
    args %<>% sub(".Data( )?\\=", "", .)
    .localEval(.makeExpression("prototype", args))
  }
  
  .mergeProtoClasses <- function(slots, proto) {
    protoClasses <- sapply(attributes(proto@object), . %>% class %>% `[`(1))
    slots[names(protoClasses)] <- protoClasses[names(protoClasses)]
    slots[slots == "name"] <- "ANY"
    slots
  }
 
  slots <- .getExplicitClass(.exprTree)
  proto <- .getPrototype(.exprTree)
  slots <- .mergeProtoClasses(slots, proto)
  if (!.protoIsGood(proto)) rm("proto")

  Class <- .exprTree$names[1]
  contains <- if (is.na(.exprTree$names[2])) 
    character() else 
      .exprTree$names[-1]
  
  retList("ClassExpressionTree")

}

InitMethodExpressionTree <- function(.exprTree, where) {
  
  .wrapInCurlyBraces <- function(x) {
    if (length(x) == 1) c("{", gsub("^\\{|\\}$", "", x), "}")
    else x
  }
  
  .body <- .wrapInCurlyBraces(.exprTree$body)
  .body[1] <- .body[1] %p0% "\n.Object <- callNextMethod()"
  
  f <- getGeneric("initialize", where = where)
  signature <- c(.Object = .exprTree$names[1])
  definition <- makeFunDef(c(".Object", "..."), .body, where)
  
  retList("MethodExpressionTree")
  
}

ConstExpressionTree <- function(.exprTree, envir) {
 
  .getConstArgs <- function(.exprTree) {
    args <- .exprTree$argDefaults[names(.exprTree$argDefaults) %without% ".Data"]
    args <- ifelse(is.na(args), 
                   names(args), 
                   paste(names(args), .exprTree$argDefaults, sep = "="))
    c(args, "...")
  }
  
  .getConstArgsNew <- function(.exprTree) {
    args <- names(.exprTree$argDefaults) %without% ".Data"
    c("'" %p0% .exprTree$names[1] %p0% "'", paste(args, args, sep = "="), "...")
  } 
  
  args <- .getConstArgs(.exprTree)
  body <- c("new(", paste(.getConstArgsNew(.exprTree), collapse = ", "), ")")
  
  retList("ConstExpressionTree")
  
}
