#' Define a Reference Class
#' 
#' This is a wrapper around \code{\link{setRefClass}}. All arguments to \code{setRefClass} are defined in an expression which improves readability of the code. Besides that, no additional features are added.
#' 
#' @param expr an expression
#' 
#' @export
#' @examples
#' \dontrun{
#'   vignette("Introduction", "aoos")
#' }
Class <- function(expr) {
  
  mc <- match.call()
  e <- new.env()
  eval(mc$expr, e)
  argsList <- as.list(e)
  
  contains <- as.character(argsList$contains)
  argsList$contains <- NULL
  Class <- as.character(argsList$Class)
  argsList$Class <- NULL
  
  argsList <- combineListElements(
    argsList,
    sapply(argsList, Negate(function(e) inherits(e, what = "function"))),
    "fields")
  
  argsList <- combineListElements(
    argsList,
    sapply(argsList, inherits, what = "function"),
    "methods")
    
  argsList$contains <- contains
  argsList$Class <- Class
  argsList$where <- parent.frame()

  do.call(setRefClass, args = argsList)
  
}


combineListElements <- function(l, ind, name) {
  newElement <- l[ind]
  l[ind] <- NULL
  l[[name]] <- newElement
  l
}

