# collect_parameters dispatch works

    Code
      collect_parameters(1)
    Condition
      Error in `collect_parameters()`:
      ! There is no `collect_parameters()` method currently implemented for `numeric` objects.

---

    Code
      collect_parameters(mtcars)
    Condition
      Error in `collect_parameters()`:
      ! There is no `collect_parameters()` method currently implemented for `data.frame` objects.

# collect_parameters errors informatively with bad arguments

    Code
      collect_parameters(st_reg_1, "the first one")
    Condition
      Error in `collect_params()`:
      ! The `candidates` argument to `collect_parameters()` must be the name given to a set of candidates added with `add_candidates()`.

---

    Code
      collect_parameters(stacks(), "all of them")
    Condition
      Error in `collect_params()`:
      ! The `candidates` argument to `collect_parameters()` must be the name given to a set of candidates added with `add_candidates()`.

