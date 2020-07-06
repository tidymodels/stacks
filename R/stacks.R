#' Initialize a data stack
#' 
#' Initializes a `data_stack` object. Principally, `data_stack`s are just
#' tibbles, where the first column gives the true outcome in the assessment set,
#' and the remaining columns give the predictions from each candidate ensemble 
#' member. (When the outcome is numeric, there’s only one column per ensemble 
#' member. Multi-way classification requires more columns.) They also bring 
#' along a few extra attributes to keep track of model definitions, resamples,
#' and training data.
#' 
#' @param ... Additional arguments. Currently ignored.
#' 
#' @return A `data_stack` object.
#' 
#' @seealso [add_members()]
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

  stack <- 
    structure(
      stack,
      class = c("data_stack", class(stack))
    )
  
  data_stack_constr(stack)
}
