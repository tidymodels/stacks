context("stack_add")

test_that("stack can add candidates (regression)", {
  expect_equal(
    st_0 %>% stack_add(reg_res_svm),
    st_reg_1
  )
  
  expect_equal(
    st_reg_1 %>% stack_add(reg_res_sp),
    st_reg_2
  )
  
  expect_true(data_stack_constr(st_0))
  expect_true(data_stack_constr(st_reg_1))
  expect_true(data_stack_constr(st_reg_2))
})

test_that("stack can add candidates (multinomial classification)", {
  expect_equal(
    st_0 %>% stack_add(class_res_rf),
    st_class_1
  )
  
  expect_equal(
    st_class_1 %>% stack_add(class_res_nn),
    st_class_2
  )
  
  expect_true(data_stack_constr(st_class_1))
  expect_true(data_stack_constr(st_class_2))
})

test_that("stack can add candidates (two-way classification)", {
  expect_equal(
    st_0 %>% stack_add(log_res_rf),
    st_log_1
  )
  
  expect_equal(
    st_log_1 %>% stack_add(log_res_nn),
    st_log_2
  )
  
  expect_true(data_stack_constr(st_log_1))
  expect_true(data_stack_constr(st_log_2))
})

test_that("stack won't add bad members", {
  expect_error(
    st_reg_1 %>% stack_add(reg_res_svm),
    "has the same name 'reg_res_svm'"
  )

  expect_error(
    st_0 %>%
      stack_add(reg_res_sp) %>%
      stack_add(reg_res_svm_new_folds),
    "same resampling object"
  )

  reg_res_svm_renamed <- reg_res_svm

  expect_error(
    st_reg_1 %>% stack_add(reg_res_svm_renamed),
    "new candidate member 'reg_res_svm_renamed' is the same as the existing"
  )
  
  expect_error(
    st_reg_1 %>% stack_add(log_res_nn),
    "has outcome variable sex, while the stack's outcome variable is body_mass_g"
  )
  
  st_reg_1_new_train <- st_reg_1
  attr(st_reg_1_new_train, "train") <- attr(st_reg_1, "train")[-1,]
  expect_error(
    st_reg_1_new_train %>% stack_add(reg_res_lr),
    "member, `reg_res_lr`, uses different training data"
  )
})

test_that("model definition naming works as expected", {
  st_reg_1_newname <- 
    stacks() %>%
    stack_add(reg_res_svm, name = "boop")

  st_class_1_newname <-
    stacks() %>%
    stack_add(class_res_rf, name = "boop")
    
  st_log_1_newname <-
    stacks() %>%
    stack_add(log_res_rf, name = "boop")
  
  expect_true(ncol_with_name(st_reg_1, "reg_res_svm") > 0)
  expect_true(ncol_with_name(st_class_1, "class_res_rf") > 0)
  expect_true(ncol_with_name(st_log_1, "log_res_rf") > 0)
  
  expect_true(ncol_with_name(st_reg_1_newname, "boop") > 0)
  expect_true(ncol_with_name(st_class_1_newname, "boop") > 0)
  expect_true(ncol_with_name(st_log_1_newname, "boop") > 0)
  
  expect_equal(ncol_with_name(st_reg_1_newname, "reg_res_svm"), 0)
  expect_equal(ncol_with_name(st_class_1_newname, "class_res_rf"), 0)
  expect_equal(ncol_with_name(st_class_1_newname, "log_res_rf"), 0)
  
  expect_error(
    st_reg_1 %>% 
      stack_add(reg_res_sp, "reg_res_svm"),
    "has the same name"
  )
  
  expect_error(
    st_class_1 %>% 
      stack_add(class_res_nn, "class_res_rf"),
    "has the same name"
  )
  
  expect_error(
    st_log_1 %>% 
      stack_add(log_res_nn, "log_res_rf"),
    "has the same name"
  )
})
