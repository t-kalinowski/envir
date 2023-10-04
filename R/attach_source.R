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
#' @param parent R environment. If an environment specified by `name` is not
#'   already attached, then the supplied R scripts are first sourced into a new
#'   environment with the supplied parent. The default of `globalenv()` enables
#'   calling `library()` in the scripts and having the subsequent
#'   code in the scripts "see" the attached packages.
#' @param warn.conflicts logical. If TRUE (the default), print warnings about
#'   objects in the attached environment that that are masking or masked by
#'   other objects of the same name. If the environment specified by `name` is
#'   was attached previously, then only newly defined objects are warned about.
#'   N.B., Even though the name is `warn.conflicts`, the messages about
#'   conflicts are not `warning()`s but `packageStartupMessage()`s, and can be
#'   suppressed with `suppressPackageStartupMessages()`
#' @param mask.ok character vector of names of objects that can mask objects on
#'   the search path without signaling a warning if `warn.conflicts` is `TRUE`.
#'   The sourced R script can also define `.mask.OK` in the R environment, which
#'   has the same effect as passing it as an argument.
#'
#' @return The attached environment, invisibly.
#' @seealso [import_from], [set_library_default_pos]
#' @export
attach_source <- function(..., # files_andor_dirs,
                          name = as_tidy_env_name(c(...), prefix = "source:"),
                          recursive = FALSE,
                          pos = 2L,
                          chdir = FALSE,
                          warn.conflicts = TRUE,
                          mask.ok = NULL,
                          parent = .GlobalEnv) {

  if (!(is.character(name) && identical(length(name), 1L)))
    stop("`name` must be a string")


  if (name %in% search()) {
    # env is already attached

    attached_env <- as.environment(name)
    mask.ok <- c(mask.ok, names(attached_env)) # don't warn about objects already masked
    include(c(...), envir = attached_env, chdir = chdir, recursive = recursive)

  } else if (is.null(parent)) {
    # user is opt-ing out of new.env(parent=globalenv()), want "old-style" attach_source()
    # where the script is literally sys.source()'d in the already attached env

    attached_env <- (attach)(NULL, pos = pos, name = name, warn.conflicts = FALSE)
    include(c(...), envir = attached_env, chdir = chdir, recursive = recursive)

  } else {
    # default code path under typical usage: create a new.env(), then attach it.
    # (N.B: attach(env) doesn't actually modify env; it creates a
    # new (attached) env then shallow copies all the bindings)

    # This approach allows R scripts to contain library() calls and function
    # definitions, and have the functions defined in the scripts "see" the
    # library().
    envir <- new.env(parent = parent)
    attr(envir, "name") <- name # a visible reminder when printing functions
    include(c(...), envir = envir, chdir = chdir, recursive = recursive)
    attached_env <- (attach)(envir, pos = pos, name = name, warn.conflicts = FALSE)
  }

  if (warn.conflicts) {
    if (exists(".conflicts.OK", envir = attached_env, inherits = FALSE)) {
      .conflicts.OK <- get(".conflicts.OK", envir = attached_env, inherits = FALSE)
      # attach() only checks for exists(".conflicts.OK"), and if TRUE, skips
      # warnings alltogether. We allow for defining .conflicts.OK to a character
      # vector of names to specifically not warn about -- essentially an alias
      # for .mask.OK
      if (is.character(.conflicts.OK) &&
          length(.conflicts.OK) &&
          length(intersect(.conflicts.OK, names(attached_env)))) {
        mask.ok <- c(mask.ok, .conflicts.OK)
      } else {
        return(invisible(attached_env))
      }
    }

    mask.ok <- c(mask.ok, get0(".mask.OK", envir = attached_env,
                               inherits = FALSE))

    warn_about_conflicts(attached_env, ignore = unique(mask.ok))
  }


  invisible(attached_env)
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
      packageStartupMessage(.maskedMsg(
        masked,
        pkg = sprintf("%s (pos = %i)", sp[i], i),
        by = i < envir_pos
      ))
      already_warned_about <- c(already_warned_about, masked)
    }
}

