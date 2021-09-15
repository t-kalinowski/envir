#' import objects
#'
#' This is inspired by the python idiom `from module import object as new_name`.
#'
#' @note If `x` is a package name, then no check is performed to ensure the
#'   object being imported is an exported function. As such, `import_from()` can
#'   be used to access package internal objects, though doing so is usually bad
#'   practice.
#'
#' @param x a bare symbol name of a package, a character vector of filepaths, an
#'   environment (which could be a python module), or any object with `names`
#'   and `[[` methods defined.
#' @param ... objects to import from x into `.into`. if named, the name will be
#'   the the new name after import. Alternatively, you can also supply the
#'   wildcard string "*" or "**", along with some additional overrides. See
#'   examples for details.
#' @param .into An \R environment, or something coercible to one by
#'   [`as.environment`], or a character string that is the name of a
#'   (potentially new) attached environment. The default is the current frame.
#' @param .parent,.chdir,.recursive Only applicable if `x` is a character vector
#'   of filepaths to R scripts, in which case these are passed on to [include]
#'   (`chdir`, `recursive`) or [new.env]`(parent)`
#' @param .pos Only applicable if `.into` is a string that is the name of a new
#'   environment that will be attached, in which case this will be the position
#'   on new environment on the search path.
#' @param .overwrite One of `"warn"`, `"error"` or `"ignore"`. Can also be a
#'   boolean `TRUE` (same as `"ignore"`) or `FALSE` (same as `"error"`). What
#'   should be done if the requested import operation would overwrite an
#'   existing object. Character arguments can be abbreviated as partial matching
#'   is performed.
#' @return The \R environment or object that `x` resolved to, invisibly.
#' @export
#'
#' @examples
#' show_whats_imported <- function(...) {
#'   import_from(...)
#'   setdiff(names(environment()), "...")
#' }
#'
#' ## Importing from an R package
#' # import one object
#' show_whats_imported(envir, include)
#'
#' # rename an object on import
#' show_whats_imported(envir, sys_source = include)
#'
#' # import all NAMESPACE exports
#' show_whats_imported(envir, "*")
#' show_whats_imported(envir) # missing `...` is interpreted as "*"
#'
#' # import all NAMESPACE exports, except for `include`
#' show_whats_imported(envir, "*", -include)
#'
#' # import all NAMESPACE exports, except rename `include` to `sys_source`
#' show_whats_imported(envir, "*", sys_source = include)
#'
#' # exclude more than one
#' show_whats_imported(envir, "*", -include, -attach_eval)
#' show_whats_imported(envir, "*", -c(include, attach_eval))
#'
#' # import all NAMESPACE exports, also one internal function names `find_r_files`
#' show_whats_imported(envir, "*", find_r_files)
#'
#' # import ALL package functions, including all internal functions
#' show_whats_imported(envir, "**")
#'
#' # import ALL objects in the package NAMESPACE, including R's NAMESPACE machinery
#' show_whats_imported(envir, "***")
#'
#'
#' ## Importing from R files
#' # setup
#' dir.create(tmpdir <- tempfile())
#' owd <- setwd(tmpdir)
#' writeLines(c("useful_function <- function() 'I am useful'",
#'              ".less_useful_fn <- function() 'less useful'"),
#'            "my_helpers.R")
#'
#' # import one function by name
#' show_whats_imported("my_helpers.R", useful_function)
#'
#' # import all objects whose names don't start with a "." or "_"
#' show_whats_imported("my_helpers.R", "*")
#'
#' # import all objects
#' show_whats_imported("my_helpers.R", "**")
#'
#' # if the filepath to your scripts is stored in a variable, supply it in a call
#' x <- "my_helpers.R"
#' try(show_whats_imported(x)) # errors out, because no package 'x'
#' # to force the value to be used, just supply it as a call rather than a bare symbol.
#' # the simplest call can be just wrapping in () or {}
#' show_whats_imported({x})
#' show_whats_imported((x))
#' show_whats_imported(c(x))
#' show_whats_imported({{x}}) # tidyverse style unquoting
#'
#' ## Importing R objects
#'
#' # if you have an actual R object that you want to import from, you will
#' # have to supply it in a call
#' x <- list(obj1 = "one", obj2 = "two")
#' show_whats_imported({x})
#'
#' \dontrun{
#'   # don't run this so we don't take a reticulate dependency
#'   import_from(reticulate, py_module = import) # rename object on import
#'
#'   # import one object
#'   show_whats_imported(py_module("numpy"), random)
#'
#'   # to prevent automatic conversion
#'   show_whats_imported(py_module("numpy", convert = FALSE), random)
#'
#'   # import all objects that don't begin with a `_`
#'   # by default, other modules found in the module are also not imported
#'   show_whats_imported(py_module("glob"), "*")
#'
#'   # to import EVERYTHING pass "**"
#'   # now includes modules that your modules imported, like `os`
#'   show_whats_imported(py_module("glob"), "**")
#'
#'   rm(py_module) # clean up
#' }
#'
#' # cleanup
#' setwd(owd)
#' unlink(tmpdir, recursive = TRUE)
#' rm(show_whats_imported, tmpdir, owd)
import_from <- function(x, ..., .into = parent.frame(),
                        .parent = .GlobalEnv,
                        .overwrite = interactive(),
                        .chdir = FALSE, .recursive = FALSE, .pos = 2L) {

  .into <- as_maybe_attached_env(.into, .pos)

  from <- if (is.symbol(x_expr <- substitute(x)))
    getNamespace(x_expr)
  else if (is.character(x))
    include(x, envir = new.env(parent = .parent),
            chdir = .chdir, recursive = .recursive)
  else if (is.environment(x) || !is.null(names(x)))
    x
  else
    stop(
      "Failed to resolve object to import from.\n",
      "`x` must be a bare symbol of a package name, a vector of file paths, an R environment, or an object with names\n",
      "`x` expression: ", deparse(x_expr), "\n",
      c("typeof(x): ", typeof(x),
        "\nclass(x): ", class(x),
        "\nformat(x):", format(x)))

  imports <- eval(substitute(alist(...)))
  if (!length(imports))
    imports <- list("*")

  is_wildcard <-
    is.character(wildcard <- imports[[1L]]) &&
    wildcard %in% c("*", "**", "***")

  imports <-
    if (is_wildcard)
      resolve_wildcard_imports(from, wildcard, overrides = imports[-1L])
  else {
    check_all_symbols(imports)
    complete_names(vapply(imports, as.character, ""))
  }

  check_requested_imports_valid(from, imports)
  check_overwrite(.overwrite, .into, names(imports))

  # avoiding mget() here: in R-4.1 it returns a list where
  # !is.null(names(names(objs))). Seems like a bug but didn't investigate further
  objs <- if (isNamespace(from))
    # inherits=TRUE, so can resolve package imports + lazydata
    lapply(imports, get, envir = from)
  else
    # can't use mget()/get()/eval() because `from` might be a python module
    # also, want inherits=FALSE behavior
    lapply(imports, function(nm) from[[nm]])

  # don't import imported python modules from python modules, dawg
  if(is_wildcard && wildcard == "*" && inherits(from, "python.builtin.module"))
    for(nm in setdiff(names(objs), attr(imports, "overrides", TRUE)))
      if(inherits(objs[[nm]], c("python.builtin.module", "__future__._Feature")))
        objs[[nm]] <- NULL


  list2env(objs, .into)

  invisible(from)
}


check_overwrite <- function(action, dest, new_bindings) {

  action <-
    if(isTRUE(action)) "ignore"
  else if (isFALSE(action)) "error"
  else match.arg(action, c("warn", "error", "ignore"))

  if (action == "ignore") return()

  already_exists <- vapply(new_bindings, function(nm)
    exists(nm, envir = dest, inherits = FALSE), TRUE)

  if (any(already_exists)) {
    w <- as.integer(max(80L, width.cutoff = 0.9 * getOption("width")))

    cl <- deparse(sys.call(1L), "\n", width.cutoff = w, nlines = 5L)
    cl <- str_collapse(cl, prefix = "In call:")

    msg <- str_collapse(strwrap(
      initial = "Bindings already exist in destination environment:",
      paste0("`", new_bindings[already_exists], "`", collapse = ", "),
      width = w, exdent = 4L))
    msg <- paste0(cl, "\n", msg)

    switch(action,
           warn = warning(msg, "\nThey have been overwritten.", call. = FALSE),
           error = stop(msg, call. = FALSE))
  }
}

check_all_symbols <- function(x) {
  if (!all(valid <- vapply(x, is.symbol, TRUE)))
    stop(
      'Values supplied to `...` must bare symbols when not using a wildcard',
      "received: ", x[!valid])
}



check_requested_imports_valid <- function(from, imports) {
  # some checks

  # check if object exists
  if (!isNamespace(from) && # get() used on NS throws an error if obj doesn't exist
      anyNA(mch <- match(imports, names_from <- names(from))))
    stop(sprintf(
      ngettext(
        sum(no_match <- is.na(mch)),
        "Object by this name does not exist: %s",
        "Objects by these names do not exist: %s"
      ),
      paste0("`", unname(imports)[no_match], "`", collapse = ", ")
    ))


  if(is.environment(from))
    return()

  # check if requested name is not unique in the originating frame
  mch <- match(names_from, imports)
  mch <- mch[!is.na(mch)]
  if(anyDuplicated(mch)) {
    stop(sprintf(
      ngettext(
        sum(dups <- duplicated(mch) & !is.na(mch)),
        "Name is not unique: %s",
        "Names are not unique: %s"
      ),
      paste0("`", names_from[dups], "`", collapse = ", ")
    ))
  }
}


expand_wildcard_imports_from_namespace <- function(from, wildcard) {

  imports <- getNamespaceExports(from)

  if (wildcard >= 2L)
    imports <- unique(c(imports, names(from)))
    # not all exports in NS, some are exported imports

  if (wildcard < 3L)
    imports <- setdiff(imports, c(
      ".__NAMESPACE__.",
      ".__S3MethodsTable__.",
      ".packageName",
      ".First.lib",
      ".Last.lib",
      ".onLoad",
      ".onAttach",
      ".onDetach",
      "library.dynam.unload",
      ".conflicts.OK",
      ".noGenerics"
    ))

  imports
}


expand_wildcard_imports_from_object <- function(from, wildcard) {

  imports <- names(from)

  if(wildcard < 2L)
    imports <- grep("^[^._]", imports, value = TRUE)

  imports
}



resolve_wildcard_imports <- function(from, wildcard, overrides) {
  if(is.character(wildcard) && !wildcard %in% c("*", "**", "***"))
    stop('wildcard must be one of: "*", "**", "***"\nReceived: ', wildcard)

  wildcard <- switch(wildcard, "*" = 1L, "**" = 2L, "***" = 3L)

  imports <- if (isNamespace(from))
    expand_wildcard_imports_from_namespace(from, wildcard)
  else
    expand_wildcard_imports_from_object(from, wildcard)

  # overrides can be bare symbols, named bare symbols, or a bare symbols with a "-" prefix
  if(length(overrides)) {
    is_drop <- vapply(overrides,
                       function(e) is.call(e) &&
                         identical(e[[1L]], quote(`-`)) &&
                         length(e) == 2L, TRUE)
    if (any(is_drop)) {
      drops <- unlist(lapply(overrides[is_drop], function(e) {
        what <- e[[2L]]
        if (is.symbol(what))
          as.character(what)
        else if (is.call(what) &&
                 identical(what[[1L]], quote(c))) {
          if (!all(valid <- vapply(what[-1L], is.symbol, TRUE)))
            stop('Values supplied to -c(...) must bare symbols',
                 "received: ",
                 imports[!valid])
          vapply(what[-1L], as.character, "")
        }}))
      imports <- setdiff(imports, drops)
      overrides <- overrides[!is_drop]
    }
  }

  if (length(overrides)) {
    if (!all(vapply(overrides, is.symbol, TRUE)))
      stop("Values supplied to `...` must by symbols")
    overrides <- vapply(overrides, as.character, "")

    # wildcard shouldn't include objects the caller wanted renamed on import
    imports <- setdiff(imports, overrides)

    # wildcard shouldn't include objects that conflict with target names caller specified
    imports <- setdiff(imports, names(overrides))

    imports <- c(imports, overrides)
  }

  structure(complete_names(imports), overrides = overrides)
}


complete_names <- function(x){
  stopifnot(is.character(x))
  nms <- names(x)
  if (is.null(nms))
    names(x) <- x
  else if (!all(named <- nzchar(nms) & !is.na(nms))) {
    nms[!named] <- x[!named]
    names(x) <- nms
  }
  x
}


str_collapse <- function(x, trunc = 4L, prefix = NULL) {
  if(length(x) > trunc)
    x <- c(x[1L:(trunc-1L)], "    [... <truncated>]")
  x <- c(prefix, x)
  paste0(x, collapse = "\n")
}

