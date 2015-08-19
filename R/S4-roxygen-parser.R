#' Parser for roxygen documentation
#' 
#' These functions are used by roxygen2 for generating documentation. 
#' 
#' @param call a call
#' @param env an environment
#' @param block is ignored
#' 
#' @rdname parser
#' @export
"parser_%m%" <- function(call, env, block) {
  value <- eval(call, env)
  roxygen2::object(value)
}

#' @export
#' @rdname parser
"parser_%g%" <- function(call, env, block) {
  value <- eval(call, env)
  roxygen2::object(value)
}

#' @export
#' @rdname parser
"parser_%type%" <- function(call, env, block) {
  value <- eval(call, env)
  roxygen2::object(value)
}
