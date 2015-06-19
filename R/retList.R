#' Return current environment as list
#' 
#' This functions can be used to construct a list with class attribute and merged with another list called super.
#' @param class character giving the class name.
#' @param exports character with the names to include.
#' @param super object returned by this function which should be extended.
#' @param superEnv objects in an environment which should be included into your current scope.
#' 
#' @seealso \link{ls}, \link{+.Infix}, \link{print.Print}
#' @rdname retList
#' @export
#' 
#' @examples 
#' 
#' Test <- function(.x) {
#'   force(.x)
#'   getX <- function() .x
#'   retList("Test")
#' }
#' 
#' Test(2)$getX()
#' 
#' ### Rational numbers example with infix operators and print method
#' 
#' Rational <- function(numer, denom) {
#'
#' e <- environment()
#' as.environment <- function() e
#' 
#' gcd <- function(a, b) if(b == 0) a else Recall(b, a %% b)
#'
#' g <- gcd(numer, denom)
#' numer <- numer / g
#' denom <- denom / g
#' 
#' print <- function() cat(paste0(numer, "/", denom, "\n"))
#' 
#' ".+" <- function(that) {
#'   Rational(numer = numer * that$denom + that$numer * denom,
#'            denom = denom * that$denom)
#' }
#' 
#' neg <- function() {
#'   Rational(numer = -numer,
#'            denom = denom)
#' }
#' 
#' ".-" <- function(that) {
#'   self + that$neg()
#' }
#' 
#' # Return only what should be visible from this scope:
#' self <- retList(c("Rational", "Infix", "Print"),
#'                 c("numer", "denom", "neg", "print", "as.environment"))
#' self
#' 
#' }
#' 
#' rational <- Rational(2, 3)
#' rational + rational
#' rational - rational
#' 
retList <- function(class = NULL, exports = NULL, super = NULL, superEnv = listAsEnv(super)) {
  envir <- parent.frame()
  exports <- unique(c(if (is.null(exports)) ls(envir) else exports, names(super)))
  superClasses <- if (is.null(super)) "list" else class(super)
  envMerge(superEnv, envir)
  out <- mget(exports, envir)
  class(out) <- c(class, superClasses)
  out
}

#' \code{funNames} returns the names of functions in the environment from which it is called.
#' 
#' @rdname retList
#' @export
funNames <- function() {
  envir <- parent.frame()
  funInd <- unlist(eapply(envir, is.function))
  names(funInd)[funInd]
}

#' \code{listAsEnv} returns an evironment. If x is NULL it is empty. If x contains a function, it is the environment of that (the first) function. If x only has non-function values it converts the list to an environment.
#' 
#' @param x a list 
#' 
#' @rdname retList
#' @export
listAsEnv <- function(x) {
  if (is.null(x)) return(new.env())
  if (any(sapply(x, is.function))) return(getEnvironmentOfFirstFun(x))
  list2env(x)
}

#' \code{getEnvironmentOfFirstFun} returns the environment of the first function in a list.
#' 
#' @rdname retList
#' @export
getEnvironmentOfFirstFun <- function(x) {
  environment(x[sapply(x, is.function)][[1]])
}
