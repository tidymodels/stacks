data_stack_constr <- function(data_stack) {
  check_inherits(attr(data_stack, "rs_hash"), "character")
  check_inherits(attr(data_stack, "outcome"), "character")
  check_inherits(attr(data_stack, "mode"), "character")
  check_inherits(attr(data_stack, "train"), "data.frame")
  check_inherits(attr(data_stack, "splits"), "data.frame")
  
  purrr::map(attr(data_stack, "cols_map"), check_inherits, "character")
  purrr::map(attr(data_stack, "model_defs"), check_inherits, "workflow")
  purrr::map(attr(data_stack, "model_metrics"), check_inherits, "data.frame")
  
  invisible(TRUE)
}

model_stack_constr <- function(model_stack) {
  check_inherits(model_stack[["coefs"]], "model_fit")
  check_inherits(model_stack[["equations"]], "list")
  check_inherits(model_stack[["train"]], "data.frame")
  check_inherits(model_stack[["data_stack"]], "data.frame")
  check_inherits(model_stack[["mode"]], "character")
  check_inherits(model_stack[["outcome"]], "character")
  check_inherits(model_stack[["splits"]], "data.frame")
  check_inherits(model_stack[["penalty"]], "list")
  
  purrr::map(model_stack[["model_defs"]], check_inherits, "workflow")
  purrr::map(model_stack[["cols_map"]], check_inherits, "character")
  purrr::map(model_stack[["model_metrics"]], check_inherits, "data.frame")
  purrr::map(model_stack[["member_fits"]], check_inherits, "workflow")
  
  invisible(TRUE)
}
