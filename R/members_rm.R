#' @rdname add_rm
#' @export
members_rm <- function(stack, member, ...) {
  check_evaluated(stack, member, "rm")
  check_member_rm(stack, member, deparse(substitute(member)))
  
  stack[["members"]][[member]] <- NULL
  
  stack_constr(stack)
}
