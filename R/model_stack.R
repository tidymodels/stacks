#' Initialize a model stack
#' 
#' This function initializes a model `stack` object. Principally, `stack`
#' objects contain out-of-sample predictions from ensemble members as well
#' as metadata identifying their connections.
#' 
#' @param ... Additional arguments. Currently ignored.
#' 
#' @return A model `stack` object.
#' 
#' @seealso [members_add()], [members_rm()]
#' 
#' @export
model_stack <- function(...) {
  
  res <- tibble::tibble()
  
  attr(res, "rs_hash") <- "init"
  attr(res, "outcome") <- NULL
  
  structure(
    res,
    class = c("stack", class(res))
  )
}
