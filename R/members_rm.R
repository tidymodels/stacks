#' @rdname add_rm
#' @export
members_rm <- function(stack, name, ...) {
  stack <- 
    stack %>%
    rm_members_checks(name) %>%
    rm_members(name)
  
  stack_constr(stack)
}
