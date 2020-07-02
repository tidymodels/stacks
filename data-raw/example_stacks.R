# some example data and model stacks fit using the example data for testing

# regression ---------------------------------------------------------------
reg_data_stack <-
  stacks() %>%
  add_members(reg_res_lr, reg_wf_lr) %>%
  add_members(reg_res_sp, reg_wf_sp) %>%
  add_members(reg_res_svm, reg_wf_svm)
  
reg_linear_stack <-
  reg_data_stack %>%
  linear_stack()

reg_model_stack <-
  reg_linear_stack %>%
  fit_members()

# multi-class classification -----------------------------------------------
class_data_stack <-
  stacks() %>%
  add_members(class_res_nn, class_wf_nn) %>%
  add_members(class_res_rf, class_wf_rf)

class_linear_stack <-
  class_data_stack %>%
  linear_stack()

class_model_stack <-
  class_linear_stack %>%
  fit_members()

# two-way classification ---------------------------------------------------
log_data_stack <-
  stacks() %>%
  add_members(log_res_nn, log_wf_nn) %>%
  add_members(log_res_rf, log_wf_rf)

log_linear_stack <-
  log_data_stack %>%
  linear_stack()

log_model_stack <-
  log_linear_stack %>%
  fit_members()
