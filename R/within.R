#' `within` methods for \R environments
#'
#' @param data An \R environment, or the name of a (potentially new) attached
#'   environment.
#' @param expr The bare \R expression to evaluate. Automatically quoted.
#' @param ... Ignored. Added for compatibility with the S3 generic. Throws an
#'   error if any arguments are passed to `...`
#'   attached environment.
#' @param quote A R language object. This is an escape hatch from the automatic
#'   quoting of `expr`.
#'
#' @return the \R environment, invisibly
#'
#' @method within environment
#' @export
within.environment <- function(data, expr, ..., quote = substitute(expr)) {
  if(...length()) stop("Arguments in `...` not allowed.")
  eval(quote, data)
  invisible(data)
}

#' @rdname within.environment
#' @export
#' @method within character
#' @inheritParams attach_eval
within.character <- function(data, expr, ..., pos = 2L,
                             warn.conflicts = TRUE, mask.ok = NULL,
                             quote = substitute(expr))
  attach_eval(
    name = data,
    expr = quote,
    pos = pos,
    warn.conflicts = warn.conflicts,
    mask.ok = mask.ok,
    ...
  )


