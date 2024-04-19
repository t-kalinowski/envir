# envir 0.3.0

- `attach_source()` now allows the R scripts to contain `library()` calls, and have 
  functions defined in the script "see" the `library()` calls. R scripts are now
  first sourced into a `new.env(parent = .GlobalEnv)`, and then all symbols are 
  subsequently copied over into a (different) attached environment. You can restore 
  the old behavior, where the scripts are evaluated in an already attached environment,
  with `attach_source(parent = NULL)`.
  
* `attach_source()` now allows scripts to define a `.mask.OK` object to a character 
  vector of names of objects that shouldn't be warned about when `warn.conflicts=TRUE`.
  
* `attach_source()` now respects the existence of `.conflicts.OK`, matching the
  behavior of `attach()`.

* `attach_source()` gains a `parent` argument, allow customizing the parent of 
  the environment where scripts will be evaluated. (default, `.GlobalEnv`)


# envir 0.2.2

* Regenerated documentation at request of CRAN for compatibility with HTML5.

# envir 0.2.1

* Fixed issue where `import_from` would not resolve reexported objects from packages. 

# envir 0.2.0

* Added a `NEWS.md` file to track changes to the package.
