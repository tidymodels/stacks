stack_constr <- function(stack) {
  check_chr(stack$rs_hash)
  
  # if there aren't any members left after the operation,
  # allow the object to accept tune_results from new resampling objects
  if (length(stack$members) == 0) {
    stack <- stack_init()
  }
  
  stack
}