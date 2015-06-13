#' Return your current environment as list
#' 
#' This functions can be used to construct a list with class attribute and merged with another list called super.
#' @param class character giving the class name
#' @param exports character with the names to include
#' @param super object returned by this function which should be extended
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
retList <- function(class = NULL, exports = NULL, super = list()) {
  envir <- parent.frame()
  superClasses <- if(is.null(super)) "list" else class(super)
  child <- if(is.null(exports)) as.list(envir) else mget(exports, envir)
  super[names(child)] <- child
  class(super) <- c(class, superClasses)
  super
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