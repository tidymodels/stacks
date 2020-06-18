#' Initialize a model stack
#' 
#' This function initializes a model `stack` object. Principally, `stack`
#' objects contain a hash for the resampling object and a list of member 
#' models.
#' 
#' @param ... Additional arguments. Currently ignored.
#' 
#' @return A model `stack` object.
#' 
#' @seealso [stack_add()], [stack_rm()]
#' 
#' @export
stack_init <- function(...) {
  structure(
    list(
      rs_hash = "init",
      members = list(),
      outcome = NULL,
      coefficients = NULL
    ),
    class = "stack"
  )
}