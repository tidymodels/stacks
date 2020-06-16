#' @rdname add_rm
#' @export
stack_add <- function(stack, member, ...) {
  stack <- check_hash(stack, member)
  
  check_member_name(stack, member)
  
  stack[["members"]][[deparse(substitute(member))]] <- member
  
  stack_constr(stack)
}
