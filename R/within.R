#' `within` methods for \R environments
#'
#' @param data An \R environment, or the name of a (potentially new) attached
#'   environment.
#' @param expr The bare \R expression to evaluate. Automatically quoted.
#' @param ... Ignored. Added for compatibility with the S3 generic. Throws an
#'   error if any arguments are passed to `...`.
#' @param quote An R language object. This is an escape hatch from the automatic
#'   quoting of `expr`.
#'
#' @return The \R environment, invisibly.
#'
#' @details The only difference between `attach_eval` and `within.character` is
#'   the order of the arguments and the return value; the first
#'   returns the result of evaluating the expression, the latter the
#'   environment.
#' @note See the note in `attach_source` about a potential pitfall of evaluating
#'   code directly in an attached environment.
#'
#' @seealso [attach_eval] [attach_source] [eval] [within]
#' @method within environment
#' @export
within.environment <- function(data, expr, ..., quote = substitute(expr)) {
  if(...length())
    stop("Arguments in `...` not allowed.")
  eval(quote, data)
  invisible(data)
}

#' @rdname within.environment
#' @export
#' @method within character
#' @inheritParams attach_eval
within.character <- function(data, expr, ..., pos = 2L,
                             warn.conflicts = TRUE, mask.ok = NULL,
                             quote = substitute(expr)) {
  if (...length())
    stop("Arguments in `...` not allowed.")

  envir <- as_maybe_attached_env(data, pos)

  if (warn.conflicts) {
    mask.ok <- c(mask.ok, names(envir))
    on.exit(warn_about_conflicts(envir, ignore = mask.ok))
  }

  eval(quote, envir)
  invisible(envir)
}
