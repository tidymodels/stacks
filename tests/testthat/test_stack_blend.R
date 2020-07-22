context("stack_blend")

test_that("stack_blend works", {
  expect_true(check_inherits(st_reg_1 %>% stack_blend(), "model_stack"))
  expect_true(check_inherits(st_class_1 %>% stack_blend(), "model_stack"))
  expect_true(check_inherits(st_log_1 %>% stack_blend(), "model_stack"))
  
  expect_null(st_reg_1_[["member_fits"]])
  expect_null(st_class_1_[["member_fits"]])
  expect_null(st_log_1_[["member_fits"]])
})

test_that("penalty argument works correctly", {
  expect_true(check_inherits(st_reg_1 %>% stack_blend(10^-2), "model_stack"))
  expect_equal(
    class(all.equal(
      st_reg_1 %>% stack_blend(10^-2),
      st_reg_1 %>% stack_blend(10^-3)
    )), 
    "character"
  )
})

test_that("stack_blend can handle many resample types", {
  expect_true(
    check_inherits(
      stacks() %>% stack_add(reg_res_svm_2) %>% stack_blend(), 
      "model_stack"
    )
  )
  
  expect_true(
    check_inherits(
      stacks() %>% stack_add(reg_res_svm_3) %>% stack_blend(), 
      "model_stack"
    )
  )
  
  expect_true(
    check_inherits(
      stacks() %>% stack_add(reg_res_svm_4) %>% stack_blend(), 
      "model_stack"
    )
  )
  
  expect_true(
    check_inherits(
      stacks() %>% stack_add(reg_res_svm_5) %>% stack_blend(), 
      "model_stack"
    )
  )
})

test_that("stack_blend errors informatively with bad arguments", {
  expect_error(
    st_reg_1__ %>% stack_blend(),
    "needs to inherit from `data_stack`, but its class is"
  )
  
  expect_error(
    stacks() %>% stack_blend(),
    "the argument to `data_stack` has no candidate members"
  )
  
  expect_error(
    stacks() %>% stack_add(reg_res_lr) %>% stack_blend(),
    "only contains one candidate member."
  )
  
  expect_error(
    stacks() %>% stack_add(class_res_nn) %>% stack_blend(),
    "only contains one candidate member."
  )
  
  expect_error(
    stacks() %>% stack_add(log_res_nn) %>% stack_blend(),
    "only contains one candidate member."
  )
  
  expect_error(
    st_reg_1 %>% stack_blend(penalty = -1),
    "supply only nonnegative values"
  )
  
  expect_error(
    st_reg_1 %>% stack_blend(penalty = "lots"),
    "supplied penalty's class is `character`"
  )
  
  expect_error(
    st_reg_1 %>% stack_blend(numeric(0)),
    "Please supply one or more penalty values."
  )
})
