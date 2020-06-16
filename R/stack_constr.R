stack_constr <- function(stack) {
  
  chr_check(stack$rs_hash)
  
  # if there aren't any members left after the operation,
  # all the object to accept tune_results from new
  # resampling objects
  if (length(stack$members) == 0) {stack$rs_hash <- "init"}
  
  stack
}