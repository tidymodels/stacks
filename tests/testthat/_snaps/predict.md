# predict method errors informatively

    Code
      st_reg_1 %>% predict(penguins_test)
    Condition
      Error in `predict()`:
      ! To predict with a stacked ensemble, the supplied data stack must be evaluated with `blend_predictions()` and its member models fitted with `fit_members()` to predict on new data.

---

    Code
      st_reg_1_ %>% predict(penguins_test)
    Condition
      Error in `predict()`:
      ! The supplied model stack hasn't been fitted yet.  Please fit the necessary members with fit_members() to predict on new data.

---

    Code
      st_reg_1__ %>% predict(penguins_test, members = "for sure!")
    Condition
      Error in `predict()`:
      ! Element members needs to inherit from `logical`, but its class is `character`.

---

    Code
      st_reg_1__ %>% predict(penguins_test, opts = TRUE)
    Condition
      Error in `predict()`:
      ! Element opts needs to inherit from `list`, but its class is `logical`.

