#' @rdname add_rm
#' @export
members_rm <- function(stack, name, ...) {
  obj_name <- deparse(substitute(name))
  
  stack <- 
    stack %>%
    rm_members_checks(name, obj_name) %>%
    rm_members(name)
  
  stack_constr(stack)
}
