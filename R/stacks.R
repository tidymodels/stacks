#' Initialize a resample stack
#' 
#' Initializes a `resample_stack` object.
#' 
#' @param ... Additional arguments. Currently ignored.
#' 
#' @return A `resample_stack` object.
#' 
#' @seealso [stack_resamples()]
#' 
#' @export
stacks <- function(...) {
  
  res <- tibble::tibble()
  
  attr(res, "rs_hash") <- "init"
  attr(res, "outcome") <- NULL
  attr(res, "model_def_names") <- NULL
  attr(res, "model_def_hashes") <- NULL
  
  structure(
    res,
    class = c("stack", class(res))
  )
}
