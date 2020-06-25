#' Initialize a stack
#' 
#' This function initializes a `stack` object. Principally, `stack`
#' objects contain out-of-sample predictions from ensemble members as well
#' as metadata identifying their connections.
#' 
#' @param ... Additional arguments. Currently ignored.
#' 
#' @return A `stack` object.
#' 
#' @seealso [add_members()], [remove_members()]
#' 
#' @export
pancakes <- function(...) {
  
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
