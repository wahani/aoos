#' S3 helper classes
#' 
#' There is no formal class definition for S3. Simply add 'Infix' or 'Print' to the class attribute and it inherits the methods. It is the same as \code{\link{Binary-class}} or \code{\link{Show-class}} just for S3. This is inteded to be used with \code{\link{retList}}.
#' 
#' @param e1 lhs operand
#' @param e2 rhs operand
#' @param x an object
#' @param ... arguments passed to the local print method.
#' 
#' @details The rhs is coerced with \code{as.environment} and in that environment the binary operators must be found and named as \code{.<binaryOperator>} (see the example for \code{\link{retList}}). This is implemented for the following operators: \code{+, -, *, /, \%\%, ^, <, >, ==, >=, <=, &}.
#' 
#' @export
#' @rdname Infix
#' @seealso \link{Binary-class}, \link{retList}
print.Print <- function(x, ...) x$print(...)

#' @export
#' @rdname Infix
"+.Infix" <-  makeBinaryMethod(".+")

#' @export
#' @rdname Infix
"-.Infix" <-  makeBinaryMethod(".-")

#' @export
#' @rdname Infix
"/.Infix" <-  makeBinaryMethod("./")

#' @export
#' @rdname Infix
"%%.Infix" <-  makeBinaryMethod(".%%")

#' @export
#' @rdname Infix
"^.Infix" <-  makeBinaryMethod(".^")

#' @export
#' @rdname Infix
"<.Infix" <-  makeBinaryMethod(".<")

#' @export
#' @rdname Infix
">.Infix" <-  makeBinaryMethod(".>")

#' @export
#' @rdname Infix
"==.Infix" <-  makeBinaryMethod(".==")

#' @export
#' @rdname Infix
">=.Infix" <-  makeBinaryMethod(".>=")

#' @export
#' @rdname Infix
"<=.Infix" <-  makeBinaryMethod(".<=")

#' @export
#' @rdname Infix
"&.Infix" <-  makeBinaryMethod(".&")

#' @export
#' @rdname Infix
as.environment.Infix <- function(x) {
  x$as.environment()
}
