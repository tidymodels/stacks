# some example data and model stacks fit using the example data for testing

# regression ---------------------------------------------------------------
reg_data_stack <-
  stacks() %>%
  add_candidates(reg_res_lr) %>%
  add_candidates(reg_res_sp) %>%
  add_candidates(reg_res_svm)
  
reg_stack_linear <-
  reg_data_stack %>%
  stack_linear()

reg_model_stack <-
  reg_stack_linear %>%
  fit_members()

# multi-class classification -----------------------------------------------
class_data_stack <-
  stacks() %>%
  add_candidates(class_res_nn) %>%
  add_candidates(class_res_rf)

class_stack_linear <-
  class_data_stack %>%
  stack_linear()

class_model_stack <-
  class_stack_linear %>%
  fit_members()

# two-way classification ---------------------------------------------------
log_data_stack <-
  stacks() %>%
  add_candidates(log_res_nn) %>%
  add_candidates(log_res_rf)

log_stack_linear <-
  log_data_stack %>%
  stack_linear()

log_model_stack <-
  log_stack_linear %>%
  fit_members()
