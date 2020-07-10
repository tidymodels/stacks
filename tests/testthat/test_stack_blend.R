context("stack_blend")

test_that("stack_blend works", {
  expect_true(check_inherits(st_reg_1 %>% stack_blend(), "model_stack"))
  expect_true(check_inherits(st_class_1 %>% stack_blend(), "model_stack"))
  expect_true(check_inherits(st_log_1 %>% stack_blend(), "model_stack"))
  
  expect_null(st_reg_1_[["member_fits"]])
  expect_null(st_class_1_[["member_fits"]])
  expect_null(st_log_1_[["member_fits"]])
})