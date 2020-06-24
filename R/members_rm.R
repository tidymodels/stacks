#' @rdname add_rm
#' @export
members_rm <- function(stack, members, ...) {
  check_evaluated(stack, members, "rm")
  check_member_rm(stack, members, members)
  
  #stack[["members"]][[member]] <- NULL
  
  stack_constr(stack)
}
