# collect_parameters dispatch works

    Code
      1 %>% collect_parameters()
    Condition
      Error in `collect_parameters()`:
      ! There is no `collect_parameters()` method currently implemented for `numeric` objects.

---

    Code
      mtcars %>% collect_parameters()
    Condition
      Error in `collect_parameters()`:
      ! There is no `collect_parameters()` method currently implemented for `data.frame` objects.

# collect_parameters errors informatively with bad arguments

    Code
      st_reg_1 %>% collect_parameters("the first one")
    Condition
      Error in `collect_params()`:
      ! The `candidates` argument to `collect_parameters()` must be the name given to a set of candidates added with `add_candidates()`.

---

    Code
      stacks() %>% collect_parameters("all of them")
    Condition
      Error in `collect_params()`:
      ! The `candidates` argument to `collect_parameters()` must be the name given to a set of candidates added with `add_candidates()`.

