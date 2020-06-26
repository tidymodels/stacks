stack_constr <- function(stack, which) {
  check_chr(attr(stack, "rs_hash"))
  
  # if there aren't any members left after the operation,
  # allow the object to accept tune_results from new resampling objects
  if (length(stack) <= 1) {
    stack <- stacks()
  }
  
  stack
}
