#' @rdname add_rm
#' @export
stack_add <- function(stack, member, ...) {
  stack <- check_hash(stack, member, deparse(substitute(member)))
  
  check_member_name(stack, member, deparse(substitute(member)))
  
  stack[["members"]][[deparse(substitute(member))]] <- member
  
  stack <- set_outcome(stack, member)
  
  stack_constr(stack)
}
