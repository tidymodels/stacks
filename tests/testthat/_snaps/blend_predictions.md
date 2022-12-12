# blend_predictions errors informatively with bad arguments

    Code
      res <- st_reg_1__ %>% blend_predictions()
    Condition
      Error in `blend_predictions()`:
      ! Element data_stack needs to inherit from `data_stack`, but its class is `linear_stack`, `model_stack`, and `list`.

---

    Code
      res <- st_reg_1 %>% blend_predictions(non_negative = "Yup")
    Condition
      Error in `blend_predictions()`:
      ! Element non_negative needs to inherit from `logical`, but its class is `character`.

---

    Code
      res <- st_reg_1 %>% blend_predictions(metric = "Yup")
    Condition
      Error in `blend_predictions()`:
      ! Element metric needs to inherit from `metric_set`, but its class is `character`.

---

    Code
      res <- st_reg_1 %>% blend_predictions(metric = yardstick::accuracy)
    Condition
      Error in `blend_predictions()`:
      ! Element metric needs to inherit from `metric_set`, but its class is `class_metric`, `metric`, and `function`.

---

    Code
      res <- stacks() %>% blend_predictions()
    Condition
      Error in `blend_predictions()`:
      ! The data stack supplied as the argument to `data_stack` has no candidate members. Please first add candidates with the `add_candidates()` (`?stacks::add_candidates()`) function.

---

    Code
      res <- stacks() %>% add_candidates(reg_res_lr) %>% blend_predictions()
    Condition
      Error in `blend_predictions()`:
      ! The supplied data stack only contains one candidate member. Please add more candidate members using `add_candidates()` (`?stacks::add_candidates()`) before blending.

---

    Code
      res <- stacks() %>% add_candidates(class_res_nn) %>% blend_predictions()
    Condition
      Error in `blend_predictions()`:
      ! The supplied data stack only contains one candidate member. Please add more candidate members using `add_candidates()` (`?stacks::add_candidates()`) before blending.

---

    Code
      res <- stacks() %>% add_candidates(log_res_nn) %>% blend_predictions()
    Condition
      Error in `blend_predictions()`:
      ! The supplied data stack only contains one candidate member. Please add more candidate members using `add_candidates()` (`?stacks::add_candidates()`) before blending.

---

    Code
      res <- st_reg_1 %>% blend_predictions(penalty = -1)
    Condition
      Error in `blend_predictions()`:
      ! Please supply only nonnegative values to the penalty argument.

---

    Code
      res <- st_reg_1 %>% blend_predictions(mixture = -1)
    Condition
      Error in `blend_predictions()`:
      ! Please supply only values in [0, 1] to the mixture argument.

---

    Code
      res <- st_reg_1 %>% blend_predictions(penalty = "lots")
    Condition
      Error in `blend_predictions()`:
      ! The argument to 'penalty' must be a numeric, but the supplied penalty's class is `character`.

---

    Code
      res <- st_reg_1 %>% blend_predictions(penalty = tibble::tibble())
    Condition
      Error in `blend_predictions()`:
      ! The argument to 'penalty' must be a numeric, but the supplied penalty's class is `tbl_df`, `tbl`, and `data.frame`.

---

    Code
      res <- st_reg_1 %>% blend_predictions(numeric(0))
    Condition
      Error in `blend_predictions()`:
      ! Please supply one or more penalty values.

# process_data_stack works

    Code
      process_data_stack(data.frame(a = rep(NA, 5)))
    Condition
      Error:
      ! All rows in the data stack have at least one missing value.  Please ensure that all candidates supply predictions.

