#' Modify default attach position for `base::library()`
#'
#' This function is documented but not exported. Reach in with
#' `envir:::set_library_default_pos()` to use it.
#'
#' @details This is primarily a way to "pin" a particular environment on the
#'   search path. For example, say you have a "project_utils" environment where
#'   you've defined a variety of useful functions. To prevent future `library()`
#'   calls from masking any objects in your attached "project_utils"
#'   environment, you can modify the default `pos` argument to library.
#'
#'   ````r
#'   attach_source("project_utils.R", name = "project_utils)
#'   set_library_default_pos(after = "project_utils")
#'   library(foo) # now foo will attach after the "project_utils" environment
#'   ````
#' @param ... Ignored. Arguments must be named
#' @param after,before string; the name of the environment on the search path
#'   that library() calls should by default attach after or before.
#' @param value The value (or quoted expression) the new argument should be.
#'
#' @return the original default value of `pos`, invisibly
set_library_default_pos <- function(..., after = NULL, before = NULL, value = NULL) {
  if(...length())
    stop("Argument supplied must be named")
  if (sum(!is.null(after), !is.null(before), !is.null(value)) != 1)
    stop("Only one of `after`, `before` or `val` can be specified.")

  new_default <- if (!is.null(after))
    bquote(which.max(search() == .(after)) + 1L)
  else if(!is.null(before))
    bquote(which.max(search() == .(before)))
  else
    value

  library <- base::library
  orig_default <- formals(library)$pos
  formals(library)$pos <- new_default

  on.exit(lockBinding("library", baseenv()))
  (unlockBinding)("library", baseenv())
  assign("library", library, envir = baseenv())

  invisible(orig_default)
}
