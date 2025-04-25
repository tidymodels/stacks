#' Initialize a Stack
#'
#' @description
#'
#' The `stacks()` function initializes a `data_stack` object. Principally,
#' `data_stack`s are tibbles, where the first column gives
#' the true outcome in the assessment set, and the remaining
#' columns give the predictions from each candidate ensemble
#' member. (When the outcome is numeric, there’s only one column per candidate
#' member. For classification, there are as many columns per candidate
#' member as there are levels in the outcome variable minus 1.) They also bring
#' along a few extra attributes to keep track of model definitions, resamples,
#' and training data.
#'
#' See `?stacks_description` for more discussion of the package, generally,
#' and the `basics` vignette for a detailed walk-through of functionality.
#'
#' @param ... Additional arguments. Currently ignored.
#'
#' @return A `data_stack` object.
#'
#' @family core verbs
#'
#' @export
stacks <- function(...) {
  check_empty_ellipses(...)

  stack <- tibble::tibble()

  attr(stack, "rs_hash") <- "init_"
  attr(stack, "outcome") <- "init_"
  attr(stack, "mode") <- "init_"
  attr(stack, "train") <- tibble::tibble()
  attr(stack, "model_defs") <- list()
  attr(stack, "cols_map") <- list()
  attr(stack, "model_metrics") <- list()
  attr(stack, "splits") <- tibble::tibble()

  stack <-
    structure(
      stack,
      class = c("data_stack", class(stack))
    )

  if (data_stack_constr(stack)) {
    stack
  }
}
