#' Define a new class
#' 
#' @param name character name of the class
#' @param expr expression
#' @param contains character name of class from which to inherit
#' 
#' @export
defineClass <- function(name, expr = NULL, contains = NULL) {
  
  mc <- processMarkup(match.call())
  parentEnv <- parent.frame()
  getMember <- function() {
    e <- setEnvironment(contains, parentEnv) # name e is needed in eval(mc)
    eval(mc)
    arrangeEnvironment(e)
  }
  
  const <- function() {
    do.call("new", c(list(Class = name), .xData = getMember()))
  }
  
  setClass(name, where = parentEnv, contains = if(is.null(contains)) "oom" else contains)
  assign(name, const, envir = parentEnv)
  
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
    # This is inheritance:
    object <- get(contains, envir = parentEnv)()
    as.environment(object)
  }
}

arrangeEnvironment <- function(e) {
  allMember <- as.list(e, all.names = TRUE)
  publicMemberInd <- sapply(allMember, function(obj) inherits(obj, "publicFunction"))
  publicMember <- allMember[publicMemberInd]
    
  f <- as.environment(publicMember)
  parent.env(f) <- e
  f
}
