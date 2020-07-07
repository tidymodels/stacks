test_that("object types relate as expected", {
  atts_d <- attributes(st_reg_1)
  
  expect_true(check_inherits(st_reg_1, "data_stack"))
  expect_true(check_inherits(st_reg_1_, "model_stack"))
  
  expect_equal(atts_d$outcome, st_reg_1_[["outcome"]])
  expect_equal(atts_d$mode, st_reg_1_[["mode"]])
  expect_equal(atts_d$model_defs, st_reg_1_[["model_defs"]])
  expect_equal(atts_d$cols_map, st_reg_1_[["cols_map"]])
  expect_equal(atts_d$model_metrics, st_reg_1_[["model_metrics"]])
  expect_equal(atts_d$train, st_reg_1_[["train"]])
  
  expect_equal(tibble::as_tibble(st_reg_1), st_reg_1_[["data_stack"]])
})
