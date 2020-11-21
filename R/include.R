#' Source \R files
#'
#' @param files_andor_dirs A character vector of filepaths to \R files, or
#'   directories containing R files. Directories are searched for files that end
#'   with extension ".R" or ".r", ignoring those that start with a period (`.`)
#'   or an underscore (`_`). The found files files from each directory are
#'   sorted by their `basename()` before being sourced. Filepaths can be
#'   supplied explicitly to override the default sorting.
#' @param envir An \R environment. By default, the current \R evaluation
#'   environment.
#' @param recursive whether to search directories recursively for R files.
#'
#'
#' @details This is a vectorized wrapper around [`base::sys.source`] with some
#'   differences. Notably:
#'
#'   *  `envir` defaults to the current frame
#'
#'   *  `envir` is returned (invisibly)
#'
#'   *  `keep.source` and `keep.parse.data` default to
#'   `getOption("keep.source")` and `getOption("keep.parse.data")` respectively,
#'   instead of `getOption("keep.source.pkgs")` and
#'   `getOption("keep.parse.data.pkgs")`
#'
#'   * `toplevel.env` is set to `getOption("topLevelEnvironment", envir)`. In
#'   other words, if the option `topLevelEnvironment` is already set, it is
#'   respected.
#'
#' @return The environment `envir`, invisibly.
#' @export
#' @inheritParams base::sys.source
include <-
  function(files_andor_dirs,
           envir = parent.frame(),
           chdir = FALSE,
           recursive = FALSE) {

    if(!is.environment(envir))
      stop("`envir` must be an environment")

    cl <- call("sys.source", quote(file), envir = envir, chdir = chdir,
               keep.source = getOption("keep.source"),
               keep.parse.data = getOption("keep.parse.data"),
               toplevel.env = getOption("topLevelEnvironment", envir))
    here <- environment()

    for (file in find_r_files(files_andor_dirs, recursive = recursive))
      eval(cl, here)

    invisible(envir)
  }


find_r_files <- function(files_andor_dirs,
                        pattern = "^[^._].+\\.[Rr]$",
                        recursive = FALSE) {

  x <- normalizePath(files_andor_dirs, mustWork = TRUE)

  if (any(is_dir <- dir.exists(x))) {
    explicit_files <- x[!is_dir]
    x <- as.list(x)
    x[is_dir] <- lapply(x[is_dir], function(path) {
      files <- list.files(path, pattern = pattern,
                          full.names = TRUE, recursive = recursive)
      files <- setdiff(files, explicit_files)
      files[order(basename(files))]
    })
    x <- unique(normalizePath(unlist(x)))
  }

  x
}
