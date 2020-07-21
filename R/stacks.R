#' stacks
#' 
#' @description 
#' 
#' This help-file describes both the initialization function in the package
#' as well as the general principles behind the package.
#' 
#' The `stacks()` function initializes a `data_stack` object. Principally, 
#' `data_stack`s are just
#' tibbles, where the first column gives the true outcome in the assessment set,
#' and the remaining columns give the predictions from each candidate ensemble 
#' member. (When the outcome is numeric, there’s only one column per candidate 
#' member. For classification, there are as many columns per candidate
#' member as there are levels in the outcome variable minus 1.) They also bring 
#' along a few extra attributes to keep track of model definitions, resamples,
#' and training data.
#' 
#' See the `Details` section below for more discussion of the package, generally.
#' 
#' @param ... Additional arguments. Currently ignored.
#' 
#' @return A `data_stack` object.
#' 
#' @family core verbs
#' 
#' @includeRmd man/rmd/grammar.Rmd
#' 
#' @export
stacks <- function(...) {
  stack <- tibble::tibble()
  
  attr(stack, "rs_hash") <- "init_"
  attr(stack, "outcome") <- "init_"
  attr(stack, "mode") <- "init_"
  attr(stack, "train") <- tibble::tibble()
  attr(stack, "model_defs") <- list()
  attr(stack, "cols_map") <- list()
  attr(stack, "model_hashes") <- list()
  attr(stack, "model_metrics") <- list()
  attr(stack, "splits") <- list()

  stack <- 
    structure(
      stack,
      class = c("data_stack", class(stack))
    )
  
  if (data_stack_constr(stack)) {stack}
}
