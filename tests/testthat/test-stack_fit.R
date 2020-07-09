context("stack_fit")

test_that("stack_fit works", {
  expect_silent(
    st_reg_1_ %>% stack_fit()
  )
  
  expect_silent(
    st_class_1_ %>% stack_fit()
  )
  
  expect_silent(
    st_log_1_ %>% stack_fit()
  )
})