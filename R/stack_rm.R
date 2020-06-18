#' @rdname add_rm
#' @export
stack_rm <- function(stack, member, ...) {
  check_evaluated(stack, member, "rm")
  
  stack[["members"]][[member]] <- NULL
  
  stack_constr(stack)
}