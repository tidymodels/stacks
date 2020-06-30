data_stack_constr <- function(data_stack) {
  check_inherits(attr(data_stack, "rs_hash"), "character")
  check_inherits(attr(data_stack, "outcome"), "character")
  check_inherits(attr(data_stack, "train"), "tbl_df")
  
  purrr::map(attr(data_stack, "cols_map"), check_inherits, "character")
  purrr::map(attr(data_stack, "model_hashes"), check_inherits, "character")
  purrr::map(attr(data_stack, "model_defs"), check_inherits, "workflow")
  purrr::map(attr(data_stack, "model_metrics"), check_inherits, "tbl_df")
  
  model_stack
}

model_stack_constr <- function(model_stack) {
  check_inherits(model_stack[["coefs"]], "_elnet")
  check_inherits(model_stack[["train"]], "tbl_df")
  
  purrr::map(model_stack[["model_defs"]], check_inherits, "workflow")
  purrr::map(model_stack[["cols_map"]], check_inherits, "character")
  purrr::map(model_stack[["model_metrics"]], check_inherits, "tbl_df")
  purrr::map(model_stack[["member_fits"]], check_inherits, "workflow")
  
  model_stack
}
