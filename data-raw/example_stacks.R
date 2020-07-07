# some example data and model stacks fit using the example data for testing

# regression ---------------------------------------------------------------
reg_data_stack <-
  stacks() %>%
  stack_add(reg_res_lr) %>%
  stack_add(reg_res_sp) %>%
  stack_add(reg_res_svm)
  
reg_stack_blend <-
  reg_data_stack %>%
  stack_blend()

reg_model_stack <-
  reg_stack_blend %>%
  stack_fit()

# multi-class classification -----------------------------------------------
class_data_stack <-
  stacks() %>%
  stack_add(class_res_nn) %>%
  stack_add(class_res_rf)

class_stack_blend <-
  class_data_stack %>%
  stack_blend()

class_model_stack <-
  class_stack_blend %>%
  stack_fit()

# two-way classification ---------------------------------------------------
log_data_stack <-
  stacks() %>%
  stack_add(log_res_nn) %>%
  stack_add(log_res_rf)

log_stack_blend <-
  log_data_stack %>%
  stack_blend()

log_model_stack <-
  log_stack_blend %>%
  stack_fit()
