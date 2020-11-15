#' `within` methods for \R environments
#'
#' @param data An \R environment, or the name of a (potentially new) attached
#'   environment.
#' @param expr The \R expression to evaluate. Automatically quoted.
#' @param ... Ignored. Added for compatibility with the S3 generic. Throws an
#'   error if any arguments are passed to `...`
#' @param .pos If creating a new attached environment, the position of the newly
#'   attached environment.
#'
#' @return the \R environment, invisibly
#'
#' @method within environment
#' @export
within.environment <- function(data, expr, ...) {
  if(...length()) stop("Arguments in `...` not allowed.")
  eval(substitute(expr), data)
  invisible(data)
}

#' @rdname within.environment
#' @export
#' @method within character
within.character <- function(data, expr, ..., .pos = 2L) {
  if(...length()) stop("Arguments in `...` not allowed.")
  eval(substitute(expr), data <- get_attached_env(data, pos = .pos))
  invisible(data)
}

