#' Source R files into an attached environment
#'
#' @param ... filepaths to R files, or paths to directories containing R files.
#' @param name A string, the name for the attached environment. By default, the
#'   name is constructed from paths supplied to `...`. If the requested name is
#'   not on the search path, a new environment of this name is attached.
#' @param recursive If directories are passed to `...`, whether to search them
#'   recursively.
#' @param pos The position where to attach the environment, if creating a new
#'   one. If an environment of `name` already exists, `pos` is ignored.
#' @param chdir logical. if TRUE, the \R working directory is temporarily
#'   changed to the directory containing the file(s) being sourced.
#' @param warn.conflicts logical. If TRUE (the default), print warnings about
#'   objects in the attached environment that that are masking or masked by
#'   other objects of the same name.
#' @param mask.ok character vector of names of objects that can mask objects on
#'   the search path without signaling a warning if `warn.conflicts` is `TRUE`
#'
#' @note One subtlety that is sometimes important: the global environment or any
#'   packages attached after this is called will not on the search path for the
#'   environment where the source is evaluated. The search path of the
#'   environment the R files are sourced in is `search()[-(1:(pos-1))]`. So
#'   for, example, functions defined in the sources will not see functions from
#'   subsequent `library` calls. This is by design. However, if you want the R
#'   functions defined in the sources to have the global environment (and all
#'   other attached environments) on their search path, then you can do
#'   something like:
#'
#'   ```` attach_eval({ import_from(c(filepaths), "**") }) ````
#'
#'   Instead of sourcing files directly in the attached environment, this snippet
#'   sources the files into a new environment that inherits from `.Globalenv`,
#'   and then copies over everything to the attached environment.
#'
#'
#' @return the attached environment, invisibly.
#' @seealso import_from, set_library_default_pos
#' @export
attach_source <- function(..., # files_andor_dirs,
                          name = as_tidy_env_name(c(...), prefix = "source:"),
                          recursive = FALSE,
                          pos = 2L,
                          chdir = FALSE,
                          warn.conflicts = TRUE,
                          mask.ok = NULL) {

  if(!is.character(name) && identical(length(name), 1L))
    stop("`name` must be a string")

  envir <- get_attached_env(name)

  if (warn.conflicts)
    mask.ok <- c(mask.ok, names(envir))

  include(c(...), envir = envir, chdir = chdir, recursive = recursive)

  if (warn.conflicts)
    warn_about_conflicts(envir, ignore = mask.ok)

  invisible(envir)
}


get_attached_env <- function(x, pos = 2L) {
  if(length(x) != 1L) stop("environment requested must be a scalar")
  if (is.character(x) && !x %in% search())
    (attach)(NULL, pos = pos, name = x, warn.conflicts = FALSE)
  else
    as.environment(x)
}


as_tidy_env_name <- function(x, prefix = NULL) {
  stopifnot(is.character(x))
  x <- gsub('["\']', "", x, perl = TRUE)
  x <- gsub("\\", "/", x, fixed = TRUE)
  is_dir <- dir.exists(x)
  x[is_dir] <- sub("/?$", "/*", x[is_dir], perl = TRUE)
  x <- paste0(x, collapse = ",")
  paste0(prefix, x)
}



warn_about_conflicts <- function(envir, ignore = NULL) {
  sp <- search()
  for (envir_pos in seq_along(sp))
    if (identical(envir, pos.to.env(envir_pos)))
      break

  objs <- names(envir)
  already_warned_about <- c(character(), ignore)
  for (i in c(seq_len(envir_pos - 1L),
              seq.int(from = envir_pos + 1, to = length(sp))))
    if (length(masked <- setdiff(intersect(names(pos.to.env(i)), objs),
                                 already_warned_about))) {
      message(.maskedMsg(
        masked,
        pkg = sprintf("%s (pos = %i)", sp[i], i),
        by = i < envir_pos
      ))
      already_warned_about <- c(already_warned_about, masked)
    }
}

