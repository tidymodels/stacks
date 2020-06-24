#' @rdname add_rm
#' @export
members_add <- function(stack, member, ...) {
  check_member_add(stack, member, deparse(substitute(member)))
  
  stack <- set_hash(stack, member, deparse(substitute(member)))
  stack <- set_outcome(stack, member)
  
  stack <- collate_member(stack, member)
  
  stack_constr(stack)
}
