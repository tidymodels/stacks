# blend_predictions errors informatively with bad arguments

    Code
      st_reg_1 %>% blend_predictions(penalty = tibble::tibble())
    Condition
      Error in `blend_predictions()`:
      ! The argument to 'penalty' must be a numeric, but the supplied penalty's class is `tbl_df`, `tbl`, and `data.frame`.

