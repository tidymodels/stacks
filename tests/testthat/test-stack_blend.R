context("stack_blend")

test_that("stack_blend works", {
  expect_silent(
    st_reg_1 %>% stack_blend()
  )
  
  expect_silent(
    st_class_1 %>% stack_blend()
  )
  
  expect_silent(
    st_log_1 %>% stack_blend()
  )
})