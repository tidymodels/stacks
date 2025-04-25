# blend_predictions errors informatively with bad arguments

    Code
      res <- blend_predictions(st_reg_1__)
    Condition
      Error in `blend_predictions()`:
      ! Element data_stack needs to inherit from `data_stack`, but its class is `linear_stack`, `model_stack`, and `list`.

---

    Code
      res <- blend_predictions(st_reg_1, non_negative = "Yup")
    Condition
      Error in `blend_predictions()`:
      ! Element non_negative needs to inherit from `logical`, but its class is `character`.

---

    Code
      res <- blend_predictions(st_reg_1, metric = "Yup")
    Condition
      Error in `blend_predictions()`:
      ! Element metric needs to inherit from `metric_set`, but its class is `character`.

---

    Code
      res <- blend_predictions(st_reg_1, metric = yardstick::accuracy)
    Condition
      Error in `blend_predictions()`:
      ! Element metric needs to inherit from `metric_set`, but its class is `class_metric`, `metric`, and `function`.

---

    Code
      res <- blend_predictions(stacks())
    Condition
      Error in `blend_predictions()`:
      ! The data stack supplied as the argument to `data_stack` has no candidate members. Please first add candidates with the `add_candidates()` (`?stacks::add_candidates()`) function.

---

    Code
      res <- blend_predictions(add_candidates(stacks(), reg_res_lr))
    Condition
      Error in `blend_predictions()`:
      ! The supplied data stack only contains one candidate member. Please add more candidate members using `add_candidates()` (`?stacks::add_candidates()`) before blending.

---

    Code
      res <- blend_predictions(add_candidates(stacks(), class_res_nn))
    Condition
      Error in `blend_predictions()`:
      ! The supplied data stack only contains one candidate member. Please add more candidate members using `add_candidates()` (`?stacks::add_candidates()`) before blending.

---

    Code
      res <- blend_predictions(add_candidates(stacks(), log_res_nn))
    Condition
      Error in `blend_predictions()`:
      ! The supplied data stack only contains one candidate member. Please add more candidate members using `add_candidates()` (`?stacks::add_candidates()`) before blending.

---

    Code
      res <- blend_predictions(st_reg_1, penalty = -1)
    Condition
      Error in `blend_predictions()`:
      ! Please supply only nonnegative values to the penalty argument.

---

    Code
      res <- blend_predictions(st_reg_1, mixture = -1)
    Condition
      Error in `blend_predictions()`:
      ! Please supply only values in [0, 1] to the mixture argument.

---

    Code
      res <- blend_predictions(st_reg_1, penalty = "lots")
    Condition
      Error in `blend_predictions()`:
      ! The argument to 'penalty' must be a numeric, but the supplied penalty's class is `character`.

---

    Code
      res <- blend_predictions(st_reg_1, penalty = tibble::tibble())
    Condition
      Error in `blend_predictions()`:
      ! The argument to 'penalty' must be a numeric, but the supplied penalty's class is `tbl_df`, `tbl`, and `data.frame`.

---

    Code
      res <- blend_predictions(st_reg_1, numeric(0))
    Condition
      Error in `blend_predictions()`:
      ! Please supply one or more penalty values.

# process_data_stack works

    Code
      process_data_stack(data.frame(a = rep(NA, 5)))
    Condition
      Error:
      ! All rows in the data stack have at least one missing value.  Please ensure that all candidates supply predictions.

