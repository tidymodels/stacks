# nocov start

# Functionality copied from tune—for classed and colored terminal output.

tune_symbol_utf8 <- list("success" = "\u2713")
tune_symbol_windows <- list("success" = "\u221A")
tune_symbol_ascii <- list("success" = "v")

tune_color_dark <- list(
  symbol = list(
    "warning" = crayon::yellow,
    "go" = crayon::white,
    "danger" = crayon::red,
    "success" = crayon::green,
    "info" = crayon::blue
  ),
  message = list(
    "warning" = crayon::yellow,
    "go" = crayon::white,
    "danger" = crayon::red,
    "success" = crayon::white,
    "info" = crayon::white
  )
)

tune_color_light <- list(
  symbol = list(
    "warning" = crayon::yellow,
    "go" = crayon::black,
    "danger" = crayon::red,
    "success" = crayon::green,
    "info" = crayon::blue
  ),
  message = list(
    "warning" = crayon::yellow,
    "go" = crayon::black,
    "danger" = crayon::red,
    "success" = crayon::black,
    "info" = crayon::black
  )
)

is_latex_output <- function () {
  if (!("knitr" %in% loadedNamespaces())) {
    return(FALSE)
  }
  
  get("is_latex_output", asNamespace("knitr"))()
}

is_windows <- function () {
  .Platform$OS.type == "windows"
}

# ----------------------------------------------------------------------

.onLoad <- function(libname, pkgname) {
  ns <- rlang::ns_env("stacks")
  
  makeActiveBinding(
    "tune_symbol",
    function() {
      # If `cli.unicode` is set we use that
      opt <- getOption("cli.unicode",  NULL)
      
      if (!is.null(opt)) {
        if (isTRUE(opt)) return(tune_symbol_utf8) else return(tune_symbol_ascii)
      }
      
      # Otherwise, try to auto-detect
      if (cli::is_utf8_output()) {
        tune_symbol_utf8
      } else if (is_latex_output()) {
        tune_symbol_ascii
      } else if (is_windows()) {
        tune_symbol_windows
      } else {
        tune_symbol_ascii
      }
    },
    ns
  )
  
  makeActiveBinding(
    "tune_color",
    function() {
      opt <- getOption("tidymodels.dark",  NULL)
      
      if (!is.null(opt)) {
        if (isTRUE(opt)) {
          return(tune_color_dark)
        } else {
          return(tune_color_light)
        }
      }
      
      tune_color_light
    },
    ns
  )
}

get_tune_colors <- function() tune_color

# nocov end