#' @rdname add_rm
#' @export
stack_rm <- function(stack, member, ...) {
  stack[["members"]][[member]] <- NULL
  
  stack_constr(stack)
}