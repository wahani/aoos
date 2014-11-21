"%method%" <- function(lhs, rhs) {
  # signature
  mc <- match.call()
  argNames <- names(mc$lhs)[-1]
  signature <- sapply(argNames, function(argName) as.character(mc$lhs[[argName]][[1]]))
  defaultValues <- sapply(argNames, function(argName) as.character(mc$lhs[[argName]][[2]]))
  defaultValues <- mapply("as", defaultValues, signature, SIMPLIFY = FALSE)
  
  # function def
  fun <- function() x
  body(fun) <- substitute(rhs)
  environment(fun) <- parent.frame()
  formals(fun) <- defaultValues
  
  # name
  genericName <- as.character(mc$lhs[[1]])
  
  # If a generic named 'genericName' does not exist, it must be set!
  if(!do.call(isGeneric, list(f = genericName))) {
    
    generic <- function(x = 1) x
    formals(generic) <- as.pairlist(defaultValues)
    body(generic) <- substitute(standardGeneric(genericName))
    environment(generic) <- parent.frame()
    
    do.call(setGeneric, list(name = genericName, def = generic))
  }
  
  do.call(setMethod, args = list(f = genericName, signature = signature, definition = fun))
  
}

newMethod(x = numeric(2)) %method% {
  x
}

tmp <- getGeneric("newMethod")

formals(tmp@.Data)

pairlist(x = 2, y = 1)

