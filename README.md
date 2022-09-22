
# envir

<!-- badges: start -->
<!-- badges: end -->

envir is a small R package with a handful of functions designed for
managing R environments. 

The main exported functions are:

- **`import_from(x, ...)`**: Import objects from `x` into the current environment.
  `x` can be:

  -  A package name
  -  Filepaths to R scripts
  -  Any object with `names` and `[[` methods.


  Usage example:
  ```
  import_from(magrittr, `%<>%`, `%>%`)
  import_from(dplyr) # all exported functions
  import_from("my_script.R", useful_function)
  ```
See `?import_from` for extended usage examples.

- **`include()`**: A vectorized wrapper around `base::sys.source` that defaults to sourcing in the current frame.

- **`attach_source()`** and **`attach_eval()`**: Evaluate an R script or expression in an attached environment

- **`within()`** S3 methods for R environments.


## Installation

You can install the released version of envir from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("envir")
```

Or install the development version with:

``` r
if (!requireNamespace("remotes", quietly = TRUE)) 
  install.packages("remotes")
remotes::install_github("t-kalinowski/envir")
```


## Related work

R has excellent support for managing environments already, but some of the
defaults encourage usage patterns that don't scale well to larger code bases.

Other solutions developed for managing R environments:

+  [modules](https://cran.r-project.org/package=modules)
+  [import](https://cran.r-project.org/package=import)
+  [box](https://cran.r-project.org/package=box)

