# add_candidates errors informatively with bad arguments

    Code
      add_candidates(reg_res_svm, "svm")
    Condition
      Error in `add_candidates()`:
      ! It looks like the first argument inherits from `tune_results`, `tbl_df`, `tbl`, and `data.frame` rather than `data_stack`.  Did you accidentally supply the candidate members as the first argument?  If so, please supply the output of `stacks()` (`?stacks::stacks()`) or another `add_candidates()` (`?stacks::add_candidates()`) call as the argument to `data_stack`.

---

    Code
      st_reg_2 %>% add_candidates("howdy")
    Condition
      Error in `add_candidates()`:
      ! The second argument to `add_candidates()` (`?stacks::add_candidates()`) should inherit from one of `tune_results` (`?tune::tune_grid()`) or `workflow_set` (`?workflowsets::workflow_set()`), but its class is `character`.

---

    Code
      stacks() %>% add_candidates(reg_res_sp, reg_res_svm)
    Condition
      Error in `add_candidates()`:
      ! The inputted `name` argument looks like a tuning/fitting results object that might be supplied as a `candidates` argument. Did you try to add more than one set of candidates in one `add_candidates()` (`?stacks::add_candidates()`) call?

---

    Code
      stacks() %>% add_candidates(reg_res_sp, TRUE)
    Condition
      Error in `add_candidates()`:
      ! Element name needs to inherit from `character`, but its class is `logical`.

---

    Code
      add_candidates("howdy", reg_res_svm)
    Condition
      Error in `add_candidates()`:
      ! Element data_stack needs to inherit from `data_stack`, but its class is `character`.

---

    Code
      st_reg_1 %>% add_candidates(reg_res_svm)
    Condition
      Error:
      ! The new model definition has the same name 'reg_res_svm' as an existing model definition.

---

    Code
      st_reg_1 %>% add_candidates(reg_res_sp, "reg_res_svm")
    Condition
      Error:
      ! The new model definition has the same name 'reg_res_svm' as an existing model definition.

---

    Code
      st_0 %>% add_candidates(reg_res_sp) %>% add_candidates(reg_res_svm_2)
    Condition
      Error:
      ! It seems like the new candidate member 'reg_res_svm_2' doesn't make use of the same resampling object as the existing candidates.

---

    Code
      st_0 %>% add_candidates(reg_res_sp) %>% add_candidates(reg_res_svm_3)
    Condition
      Error:
      ! It seems like the new candidate member 'reg_res_svm_3' doesn't make use of the same resampling object as the existing candidates.

---

    Code
      res <- st_reg_1_new_train %>% add_candidates(reg_res_lr)
    Condition
      Error:
      ! The newly added candidate member, `reg_res_lr`, uses different training data than the existing candidates.

---

    Code
      stacks() %>% add_candidates(reg_res_lr_bad)
    Condition
      Error in `add_candidates()`:
      ! The inputted `candidates` argument was not generated with the appropriate control settings. Please see `control_stack()` (`?stacks::control_stack()`).

---

    Code
      stacks() %>% add_candidates(reg_res_lr_bad2)
    Condition
      Error in `add_candidates()`:
      ! The inputted `candidates` argument was not generated with the appropriate control settings. Please see `control_stack()` (`?stacks::control_stack()`).

---

    Code
      stacks() %>% add_candidates(reg_res_lr) %>% add_candidates(reg_res_lr_renamed)
    Condition
      Warning:
      Predictions from 1 candidate were identical to those from existing candidates and were removed from the data stack.
    Output
      # A data stack with 2 model definitions and 1 candidate member:
      #   reg_res_lr: 1 model configuration
      #   reg_res_lr_renamed: 0 model configurations
      # Outcome: latency (numeric)

---

    Code
      stacks() %>% add_candidates(log_res)
    Condition
      Error:
      ! The supplied candidates were tuned/fitted using only metrics that rely on hard class predictions. Please tune/fit with at least one class probability-based metric, such as `roc_auc` (`?yardstick::roc_auc()`).

---

    Code
      stacks() %>% add_candidates(reg_res_lr)
    Condition
      Error:
      ! The stacks package does not support stacking models with mode "censored regression".

# model definition naming works as expected

    Code
      st_reg_1 %>% add_candidates(reg_res_sp, "reg_res_svm")
    Condition
      Error:
      ! The new model definition has the same name 'reg_res_svm' as an existing model definition.

---

    Code
      st_class_1 %>% add_candidates(class_res_nn, "class_res_rf")
    Condition
      Error:
      ! The new model definition has the same name 'class_res_rf' as an existing model definition.

---

    Code
      st_log_1 %>% add_candidates(log_res_nn, "log_res_rf")
    Condition
      Error:
      ! The new model definition has the same name 'log_res_rf' as an existing model definition.

---

    Code
      st_reg_1 <- stacks() %>% add_candidates(reg_res_svm, name = "beep bop")
    Message
      The inputted `name` argument cannot prefix a valid column name. The data stack will use 'beep.bop' rather than 'beep bop' in constructing candidate names.

# stacks can add candidates via workflow sets

    ! Some elements of the supplied workflow set failed to evaluate with resamples.
    i The workflow with ID `reg_lr` will be excluded from the data stack.

---

    ! Some elements of the supplied workflow set failed to evaluate with resamples.
    i The workflows with ID `reg_lr` and `reg2_svm` will be excluded from the data stack.

---

    The supplied workflow set must be fitted to resamples with `workflow_map()` (`?workflowsets::workflow_map()`) before being added to a data stack.

