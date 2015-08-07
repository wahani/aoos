#' Wrapper for writing S4 generics and methods
#' 
#' These are two wrappers around \code{setGeneric} and \code{setMethod}. A relevant difference is that generics and methods are stored in the environment in which  \code{\%g\%} and \code{\%m\%} are called and not in the top-environment. Furthermore both functions have side effects in that they will call \code{\link{globalVariables}} for the arguments and name of the generic.
#' 
#' @param lhs an expression like \code{genericName(<argList>)} for \code{\%g\%} and \code{genericName(<args = signature>, <argList>)} for \code{\%m\%}.
#' @param rhs the body as an expression.
#' 
#' @export
#' @rdname S4generics
"%g%" <- function(lhs, rhs) {
  
  mc <- match.call()
  lhs <- deparse(mc$lhs)
  setGenericArgList <- list()
  setGenericArgList$name <- deleteInParan(lhs)
  setGenericArgList$where <- parent.frame()
  setGenericArgList$def <- {
    defCall <- "function" %p0% deleteBeforeParan(lhs) %p0% " 1" # template fun
    generic <- eval(parse(text = defCall))
    environment(generic) <- parent.frame()
    body(generic) <- mc$rhs
    generic
  }
  
  # Fix for R CMD check:
  globalVariables(c(
    setGenericArgList$name, 
    names(formals(setGenericArgList$def))), topenv(setGenericArgList$where)
    )
  
  invisible(do.call("setGeneric", setGenericArgList))
  
}

deleteBeforeParan <- . %>% sub("(^.*\\()", "", .) %>% paste0("(", .)
deleteEnclosingParan <- . %>% sub("\\)$", "", .) %>% sub("^\\(", "", .)
deleteInParan <- . %>% gsub("\\(.*\\)", "", .)
"%p0%" <- function(lhs, rhs) paste0(lhs, rhs)
"%p%" <- function(lhs, rhs) paste(lhs, rhs)

#' @export
#' @rdname S4generics
"%m%" <- function(lhs, rhs) {
  
  # Some helpers:
  findGenericFunction <- function(lhs, envir) {
    genericName <- deleteInParan(lhs)
    eval(parse(text = genericName), envir = envir)
  }

  splitTrim <- function(x, pattern) {
    strsplit(x, pattern) %>% unlist %>% trimws
  }
  
  collapseArgumentList <- function(names, defaults) {
    paste(names, defaults, sep = "=") %>% paste(collapse = " , ")
  }
  
  # Preparing:
  mc <- match.call()
  lhs <- deparse(mc$lhs)
  
  # This list will be used in a do.call for setMethod:
  setMethodArgList <- list()
  setMethodArgList$where <- parent.frame()
  
  # The generic function
  setMethodArgList$f <- findGenericFunction(lhs, setMethodArgList$where)
  
  # The signature:
  args <- deleteBeforeParan(lhs)
  args %<>% deleteEnclosingParan 
  args %<>% splitTrim(",")
  argNames <- sapply(as.list(args), . %>% splitTrim(., "=") %>% .[1])
  namesInGeneric <- argNames %in% names(formals(setMethodArgList$f))
  defaults <- sapply(as.list(args), . %>% splitTrim(., "=") %>% .[2])
  signature <- defaults[namesInGeneric]
  signature <- ifelse(is.na(signature), "ANY", signature)
  setMethodArgList$signature <- signature
  
  # The function/method
  defaults <- ifelse(is.na(defaults), "", defaults)
  defaults <- ifelse(namesInGeneric, "", defaults)
  
  templateFun <- function() 1
  
  formals(templateFun) <- 
    "alist(" %p0% collapseArgumentList(argNames, defaults) %p0% ")" %>%
    parse(text = .) %>%
    eval
  
  body(templateFun) <- mc$rhs
  environment(templateFun) <- setMethodArgList$where
  setMethodArgList$definition <- templateFun
  
  # Fix for R CMD check:
  globalVariables(argNames, topenv(setMethodArgList$where))
  
  invisible(do.call("setMethod", setMethodArgList))
  
}
