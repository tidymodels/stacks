#' @importFrom generics fit
#' @export
generics::fit

#' Fit an evaluated model stack.
#' 
#' @param object An evaluated model stack.
#' @inheritParams stack_init
#' 
#' @rdname fit
#' @export
fit.stack <- function(object, ...) {
  invisible(TRUE)
}