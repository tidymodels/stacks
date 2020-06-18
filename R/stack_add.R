#' @rdname add_rm
#' @export
stack_add <- function(stack, member, ...) {
  check_evaluated(stack, deparse(substitute(member)), "add")
  check_member(stack, member, deparse(substitute(member)))
  
  stack <- check_hash(stack, member, deparse(substitute(member)))
  
  stack[["members"]][[deparse(substitute(member))]] <- member
  
  stack <- set_outcome(stack, member)
  
  stack_constr(stack)
}
