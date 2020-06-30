#' Initialize a resample stack
#' 
#' Initializes a `resample_stack` object.
#' 
#' @param ... Additional arguments. Currently ignored.
#' 
#' @return A `resample_stack` object.
#' 
#' @seealso [add_members()]
#' 
#' @export
stacks <- function(...) {
  stack <- tibble::tibble()
  
  attr(stack, "rs_hash") <- "init_"
  attr(stack, "outcome") <- "init_"
  attr(stack, "model_defs") <- list()
  attr(stack, "cols_map") <- list()
  attr(stack, "model_hashes") <- list()
  attr(stack, "model_metrics") <- list()
  
  # should check recipe outcome variable if workflows are
  # eventually not included in the resampling object
  # $pre$actions$recipe$recipe$var_info
  #attr(stack, "wf_hash") <- "init_"

  stack <- 
    structure(
      stack,
      class = c("stack", class(stack))
    )
  
  stack
}
