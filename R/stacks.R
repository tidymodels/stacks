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
  stack <- list()
  
  attr(stack, "rs_hash") <- "init_"
  attr(stack, "outcome") <- "init_"
  
  
  stack[["model_defs"]] <- list()
  stack[["data"]] <- tibble::tibble()
  stack[["cols_map"]] <- list()
  stack[["coefs"]] <- list()
  
  # should check recipe outcome variable if workflows are
  # eventually not included in the resampling object
  # $pre$actions$recipe$recipe$var_info
  #attr(stack, "wf_hash") <- "init_"
  
  #attr(stack, "model_def_names") <- NULL
  #attr(stack, "model_def_hashes") <- NULL
  
  stack <- 
    structure(
      stack,
      class = c("stack", class(stack))
    )
}
