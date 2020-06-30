stack_constr <- function(stack) {
  check_inherits(attr(stack, "rs_hash"), "character")
  check_inherits(attr(stack, "outcome"), "character")
  check_inherits(attr(stack, "train"), "tbl_df")
  
  purrr::map(attr(stack, "cols_map"), check_inherits, "character")
  purrr::map(attr(stack, "model_hashes"), check_inherits, "character")
  purrr::map(attr(stack, "model_defs"), check_inherits, "workflow")
  purrr::map(attr(stack, "model_metrics"), check_inherits, "tbl_df")
  
  stack
}

ensemble_constr <- function(ensemble) {
  check_inherits(ensemble[["coefs"]], "_elnet")
  check_inherits(ensemble[["train"]], "tbl_df")
  
  purrr::map(ensemble[["model_defs"]], check_inherits, "workflow")
  purrr::map(ensemble[["cols_map"]], check_inherits, "character")
  purrr::map(ensemble[["model_metrics"]], check_inherits, "tbl_df")
  purrr::map(ensemble[["member_fits"]], check_inherits, "workflow")
  
  ensemble
}
