#' @rdname add_rm
#' @export
members_add <- function(stack, members, 
                        name = deparse(substitute(members)), ...) {
  check_chr(name)
  
  stack <- 
    stack %>%
    set_rs_hash(members, name) %>%
    set_outcome(members) %>%
    set_model_defs(members, name)
  
  stack <- collate_member(stack, members, name)
  
  stack_constr(stack)
}
