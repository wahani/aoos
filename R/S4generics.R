#' Wrapper for writing S4 generics
#' 
#' @export
#' @rdname S4generics
"%g%" <- function(lhs, rhs) {
  
  mc <- match.call()
  lhs <- deparse(mc$lhs)
  genericNameEnd <- str_locate(lhs, "\\(")[1, 2] - 1
  genericName <- str_sub(lhs, 1, genericNameEnd)
  defCall <- lhs %>% deleteBeforeParan %>% paste0("function", ., 1)
  generic <- eval(parse(text = defCall), envir = parent.frame())
  body(generic) <- mc$rhs
  
  do.call("setGeneric", list(name = genericName, def = generic, where = parent.frame()))
  
}

#' @export
#' @rdname S4generics
"%m%" <- function(lhs, rhs) {
  
  lhsDeparseArgs <- function(lhs) {
    lhs %<>% str_sub(str_locate(lhs, "\\(")[1, 2] + 1, str_length(lhs) - 1)
    lhs %<>% str_split(",")
    lhs %<>% lapply(str_split, pattern = "~|=")
    lhs %<>% do.call(what = c)
    lhs %<>% lapply(str_trim)
    lhs
  }
  
  findGenericFunction <- function(lhs, envir) {
    genericNameEnd <- str_locate(lhs, "\\(")[1, 2] - 1
    genericName <- str_sub(lhs, 1, genericNameEnd)
    eval(parse(text = genericName), envir = envir)
  }
  
  makeSignature <- function(lhs, generic) {
    argsAsChar <- lhsDeparseArgs(lhs)
    ind <- sapply(argsAsChar, "[", i = 1) %in% names(formals(generic))
    argClasses <- str_replace(sapply(argsAsChar, "[", i = 2), pattern = "\\(.*$", "")
    argClasses <- ifelse(is.na(argClasses), "ANY", argClasses)
    argClasses[ind]
  }
  
  makeDefinition <- function(lhs, mc, envir) {
    definition <- function() "emptyFunctionWillBeFilled"
    argsAsChar <- lhsDeparseArgs(lhs)
    arguments <- lapply(argsAsChar, function(arg) {
      arg[2] <- deleteBeforeParan(arg[2])
      arg <- ifelse(is.na(arg), "", arg)
      paste(arg, collapse = "=")
    }) %>% paste(collapse = ", ")
    formals(definition) <- eval(parse(text = paste0("alist(", arguments, ")")))
    environment(definition) <- envir
    body(definition) <- mc$rhs
    definition
  }
  
  mc <- match.call()
  envir <- parent.frame()
  lhs <- deparse(mc$lhs)
  
  setMethodArgList <- list()
  setMethodArgList$f <- findGenericFunction(lhs, envir)
  setMethodArgList$signature <- makeSignature(lhs, setMethodArgList$f)
  setMethodArgList$definition <- makeDefinition(lhs, mc, envir)
  
  do.call(setMethod, setMethodArgList)
  
}

deleteBeforeParan <- function(x) {
  posOfFirstP <- str_locate(x, "\\(")[1, 2]
  if (!is.na(posOfFirstP)) str_sub(x, 1, posOfFirstP - 1) <- ""
  x
}

Poly <- function() {
  ~generic(x) %g% { x }
  ~generic(x = numeric) %m% { x }
  retList("Poly")
}
