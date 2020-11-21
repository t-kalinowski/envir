#' Evaluate R expressions in an attached environment.
#'
#' @param unquoted_expr The expression to be evaluated, This is automatically
#'   quoted.
#' @param name The environment name. If an environment of that name already
#'   exists, it is reused, otherwise, a new environment is attached.
#' @param pos The position where to attach the environment, if creating a new
#'   one. If an environment of `name` already exists, `pos` is ignored.
#' @param warn.conflicts logical. If TRUE (the default), print warnings about
#'   objects in the attached environment that that are masking or masked by
#'   other objects of the same name.
#' @param ... Ignored.
#' @param mask.ok character vector of names of objects that can mask objects on
#'   the search path without signaling a warning if `warn.conflicts` is `TRUE`.
#' @param expr An R language object. This is an escape hatch from the automatic
#'   quoting of `unquoted_expr`.
#'
#' @return The result after evaluating `expr`, invisibly.
#' @export
#'
#' @examples
#' attach_eval({
#'   my_helper_funct <- function(x, y) x + y
#' })
#'
#' search() # environment "local:utils" is now attached
#' my_helper_funct(1, 1) # the local utility is now available
#'
#' detach(local:utils) # cleanup
attach_eval <- function(unquoted_expr, name = "local:utils", pos = 2L,
                        warn.conflicts = TRUE, ...,
                        expr = substitute(unquoted_expr),
                        mask.ok = NULL) {

  if (...length())
    stop("Arguments in `...` not allowed.")

  envir <- as_maybe_attached_env(name, pos)

  if (warn.conflicts) {
    mask.ok <- c(mask.ok, names(envir))
    on.exit(warn_about_conflicts(envir, ignore = mask.ok))
  }

  invisible(eval(expr, envir))
}



as_maybe_attached_env <- function(x, pos = 2L) {
  tryCatch(
    as.environment(x),
    error = function(e)
      if (is.character(x) && identical(length(x), 1L))
        (attach)(NULL, pos = pos, name = x, warn.conflicts = FALSE)
    else
      stop("Expected an environment, something coercible to one via `as.environment()`, or a string to use as the name for a newly attached environment."))
}

