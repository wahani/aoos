#' Return current environment as list
#' 
#' This functions can be used to construct a list with class attribute and merged with another list called super. What merge means can be specified with the mergeFun argument. It is intended to construct the return value (S3 class inheriting from 'list') of a function.
#' 
#' @param class character giving the class name.
#' @param public character with the names to include.
#' @param super a list to be extended.
#' @param superEnv environment where new methods will live in.
#' @param mergeFun function with two arguments. Knows how to join/merge environments - \code{mergeFun(envir, superEnv)}. Default: \link{envMerge}.
#' @param envir this is the environment you want to convert to the list. Default is the environment from which the function is called.
#' 
#' @seealso \link{ls}, \link{+.Infix}, \link{print.Print}
#' @rdname retList
#' @export
#' 
#' @examples 
#' # To get a quick overview of the package:
#' vignette("Introduction", "aoos")
#' 
#' # To get more info about retList:
#' vignette("retListClasses", "aoos")
#' 
#' # A simple class with one method:
#' Test <- function(.x) {
#'   getX <- function() .x
#'   retList("Test")
#' }
#' 
#' stopifnot(Test(2)$getX() == 2)
#' 
#' # A second example inheriting from Test
#' Test2 <- function(.y) {
#'   getX2 <- function() .x * 2
#'   retList("Test2", super = Test(.y))
#' }
#' 
#' stopifnot(Test2(2)$getX() == 2)
#' stopifnot(Test2(2)$getX2() == 4)
#' 
#' ### Rational numbers example with infix operators and print method
#' 
#' Rational <- function(numer, denom) {
#'
#'   gcd <- function(a, b) if(b == 0) a else Recall(b, a %% b)
#'
#'   g <- gcd(numer, denom)
#'   numer <- numer / g
#'   denom <- denom / g
#' 
#'   print <- function(x, ...) cat(paste0(numer, "/", denom, "\n"))
#' 
#'   ".+" <- function(that) {
#'     Rational(numer = numer * that$denom + that$numer * denom,
#'              denom = denom * that$denom)
#'   }
#' 
#'   ".-" <- function(that) {
#'     if (missing(that)) {
#'       Rational(-numer, denom)
#'     } else {
#'       .self + (-that)
#'     }
#'   }
#' 
#'   # Return only what should be visible from this scope:
#'   retList(c("Rational", "Infix", "Print"),
#'           c("numer", "denom", "neg", "print"))
#' 
#' }
#' 
#' rational <- Rational(2, 3)
#' rational + rational
#' rational - rational
retList <- function(class = NULL, public = NULL, super = NULL, superEnv = asEnv(super), mergeFun = envMerge, envir = parent.frame()) {
  public <- unique(c(if (is.null(public)) ls(envir) else public, names(super)))
  superClasses <- if (is.null(super)) "list" else class(super)
  if (length(superEnv) > 0) envir <- mergeFun(envir, superEnv)
  envir$.self <- envir
  out <- as.list(envir)[public]
  attr(out, ".self") <- envir
  class(out) <- c(class, superClasses)
  out
}

#' @details \code{funNames} returns the names of functions in the environment from which it is called.
#' 
#' @rdname retList
#' @export
funNames <- function(envir = parent.frame()) {
  funInd <- unlist(eapply(envir, is.function))
  names(funInd)[funInd]
}

#' @details \code{asEnv} returns an evironment. If x is NULL it is empty. Else if x has an attribute called \code{.self} it is this attribute which is returned. If x only has non-function values it converts the list to an environment.
#' 
#' @param x a list 
#' 
#' @rdname retList
#' @export
asEnv <- function(x) {
  if (is.null(x)) return(new.env())
  else if (!is.null(attr(x, ".self"))) return(attr(x, ".self"))
  else return(list2env(x))
}
