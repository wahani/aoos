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
  
  .exprTree <- ExpressionTree(match.call())
  .classExprTree <- ClassExpressionTree(match.call(), parent.frame())
  .initExprTree <- InitMethodExpressionTree(match.call(), parent.frame())
  .constExprTree <- ConstExpressionTree(match.call(), parent.frame())
  
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

ClassExpressionTree <- function(.mc, where) {
  
  .makeExpression <- function(funName, args) {
    parse(text = paste0(
      funName, "(", paste(args, collapse = ", "), ")"
    ))
  }
  
  .protoIsGood <- function(proto) 
    proto@dataPart || !identical(proto@slots, character())
  
  .localEval <- function(expr) eval(expr, envir = where)
  
  .exprTree <- ExpressionTree(.mc)
  .slotExprTree <- SlotExpressionTree(.mc)
  
  Class <- .exprTree$names[1]
  contains <- if (is.na(.exprTree$names[2])) 
    character() else 
      .exprTree$names[-1]
  
  slots <- .localEval(.makeExpression("list", .slotExprTree$slots)) %>% sapply(class)
  slots <- ifelse(slots == "NULL", "ANY", slots)
  if (length(slots) == 0) rm("slots")
  proto <- .localEval(.makeExpression("prototype", .slotExprTree$proto))
  if (!.protoIsGood(proto)) rm("proto")
  
  retList("ClassExpressionTree")

}

SlotExpressionTree <- function(.mc, where) {
  
  .exprTree <- ExpressionTree(.mc)
  
  .argsSplitted <- lapply(.exprTree$args, splitTrim, pattern = "=")
  .argNames <- sapply(.argsSplitted, `[`, 1)
  
  .allArgs <- .argsSplitted[!(.argNames %in% c("..."))] %>%
    ifelse(sapply(., length) == 1, lapply(., inset, 2, "NULL"), .) %>%
    sapply(paste, collapse = " = ")
  
  proto <- .allArgs %>% sub(".Data( )?\\=", "", .)
  slots <- .allArgs[!grepl(".Data", .allArgs)]
  const <- unlist(c(slots, "..."))
  .slotNames <- .argNames %without% c(".Data", "...")
  constNew <- if(length(.slotNames) == 0) "..." else c(.slotNames %p% "=" %p% .slotNames, "...")
  constNew <- c(paste0("'", .exprTree$names[1], "'"), constNew)
  
  retList("SlotExpressionTree")
  
}

InitMethodExpressionTree <- function(.mc, where) {
  
  .wrapInCurlyBraces <- function(x) {
    if (length(x) == 1) c("{", gsub("^\\{|\\}$", "", x), "}")
    else x
  }
  
  .exprTree <- ExpressionTree(.mc)
  .body <- .wrapInCurlyBraces(.exprTree$body)
  .body[1] <- .body[1] %p0% "\n.Object <- callNextMethod()"
  
  f <- getGeneric("initialize", where = where)
  signature <- c(.Object = .exprTree$names[1])
  definition <- makeFunDef(c(".Object", "..."), .body, where)
  
  retList("MethodExpressionTree")
  
}

ConstExpressionTree <- function(.mc, envir) {
  
  .exprTree <- ExpressionTree(.mc)
  .slotExprTree <- SlotExpressionTree(.mc)
  
  args <- .slotExprTree$const
  body <- c("new(", paste(.slotExprTree$constNew, collapse = ", "), ")")
  
  retList("ConstExpressionTree")
  
}
