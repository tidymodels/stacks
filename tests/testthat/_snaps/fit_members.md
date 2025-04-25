# fit_members errors informatively with a bad model_stack arg

    Code
      fit_members(st_reg_1)
    Condition
      Error in `fit_members()`:
      ! The supplied `model_stack` argument is a data stack rather than a model stack. Did you forget to first evaluate the ensemble's stacking coefficients with `blend_predictions()` (`?stacks::blend_predictions()`)?

---

    Code
      fit_members("howdy")
    Condition
      Error in `fit_members()`:
      ! Element model_stack needs to inherit from `model_stack`, but its class is `character`.

---

    Code
      out <- fit_members(st_reg_1__)
    Condition
      Warning:
      The members in the supplied `model_stack` have already been fitted and need not be fitted again.

# fit_members checks for required packages

    Code
      error_needs_install(letters[1], rep(FALSE, 1))
    Condition
      Error in `error_needs_install()`:
      ! The package a needs to be installed before fitting members.

---

    Code
      error_needs_install(letters[1:2], rep(FALSE, 2))
    Condition
      Error in `error_needs_install()`:
      ! The packages a and b need to be installed before fitting members.

---

    Code
      error_needs_install(letters[1:3], rep(FALSE, 3))
    Condition
      Error in `error_needs_install()`:
      ! The packages a, b, and c need to be installed before fitting members.

---

    Code
      fit_members(st_reg_1_)
    Condition
      Error in `fit_members()`:
      ! The packages recipes, parsnip, and kernlab need to be installed before fitting members.

