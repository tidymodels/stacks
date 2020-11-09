context("add_candidates")

load(test_path("helper_data.Rda"))

test_that("stack can add candidates (regression)", {
  skip("still some inconsistencies with stored objects")
  
  expect_equal(
    st_0 %>% add_candidates(reg_res_svm),
    st_reg_1
  )
  
  expect_equal(
    st_reg_1 %>% add_candidates(reg_res_sp),
    st_reg_2
  )
  
  expect_true(data_stack_constr(st_0))
  expect_true(data_stack_constr(st_reg_1))
  expect_true(data_stack_constr(st_reg_2))
})

test_that("stack can add candidates (multinomial classification)", {
  skip("still some inconsistencies with stored objects")
  
  expect_equal(
    st_0 %>% add_candidates(class_res_rf),
    st_class_1
  )
  
  expect_equal(
    st_class_1 %>% add_candidates(class_res_nn),
    st_class_2
  )
  
  expect_true(data_stack_constr(st_class_1))
  expect_true(data_stack_constr(st_class_2))
})

test_that("stack can add candidates (two-way classification)", {
  skip("still some inconsistencies with stored objects")
  
  expect_equal(
    st_0 %>% add_candidates(log_res_rf),
    st_log_1
  )
  
  expect_equal(
    st_log_1 %>% add_candidates(log_res_nn),
    st_log_2
  )
  
  expect_true(data_stack_constr(st_log_1))
  expect_true(data_stack_constr(st_log_2))
})

test_that("add_candidates errors informatively with bad arguments", {
  skip_on_cran()
  
  expect_error(
    add_candidates(reg_res_svm, "svm"),
    "Did you accidentally supply the candidate members as the first argument?"
  )
  
  expect_error(
    st_reg_2 %>% add_candidates("howdy"),
    "has class `character`, but it should inherit"
  )
  
  expect_error(
    stacks() %>% add_candidates(reg_res_sp, reg_res_svm),
    "Did you try to add more than one set of candidates in one `add_candidates\\(\\)"
  )
  
  expect_error(
    stacks() %>% add_candidates(reg_res_sp, TRUE),
    "Element `name` needs to inherit from `character`, but its class is `logical`."
  )
  
  expect_error(
    add_candidates("howdy", reg_res_svm),
    "needs to inherit from `data_stack`, but its class is `character`"
  )
  
  expect_error(
    st_reg_1 %>% add_candidates(reg_res_svm),
    "has the same name 'reg_res_svm'"
  )
  
  expect_error(
    st_reg_1 %>% add_candidates(reg_res_sp, "reg_res_svm"),
    "has the same name 'reg_res_svm'"
  )

  expect_error(
    st_0 %>%
      add_candidates(reg_res_sp) %>%
      add_candidates(reg_res_svm_2),
    "same resampling object"
  )
  
  expect_error(
    st_0 %>%
      add_candidates(reg_res_sp) %>%
      add_candidates(reg_res_svm_3),
    "same resampling object"
  )
  
  st_reg_1_new_train <- st_reg_1
  attr(st_reg_1_new_train, "train") <- attr(st_reg_1, "train")[-1,]
  expect_error(
    st_reg_1_new_train %>% add_candidates(reg_res_lr),
    "member, `reg_res_lr`, uses different training data"
  )
  
  reg_res_lr_ <- lin_reg_spec <-
    parsnip::linear_reg() %>%
    parsnip::set_engine("lm")
  
  reg_wf_lr <- 
    workflows::workflow() %>%
    workflows::add_model(lin_reg_spec) %>%
    workflows::add_recipe(tree_frogs_reg_rec)
  
  set.seed(1)
  
  reg_res_lr_bad <- 
    tune::fit_resamples(
      object = reg_wf_lr,
      resamples = reg_folds,
      metrics = yardstick::metric_set(yardstick::rmse),
      control = tune::control_resamples(
        save_pred = TRUE,
        save_workflow = FALSE
      )
    )
  
  expect_error(
    stacks() %>% add_candidates(reg_res_lr_bad),
    "not generated with the appropriate control settings"
  )
  
  reg_res_lr_bad2 <- 
    tune::fit_resamples(
      object = reg_wf_lr,
      resamples = reg_folds,
      metrics = yardstick::metric_set(yardstick::rmse),
      control = tune::control_resamples(
        save_pred = FALSE,
        save_workflow = TRUE
      )
    )
  
  expect_error(
    stacks() %>% add_candidates(reg_res_lr_bad2),
    "not generated with the appropriate control settings"
  )
  
  reg_res_lr_renamed <- reg_res_lr
  
  expect_warning(
    stacks() %>% add_candidates(reg_res_lr) %>% add_candidates(reg_res_lr_renamed),
    "were identical to those"
  )
})

test_that("model definition naming works as expected", {
  skip_on_cran()
  
  st_reg_1_newname <- 
    stacks() %>%
    add_candidates(reg_res_svm, name = "boop")

  st_class_1_newname <-
    stacks() %>%
    add_candidates(class_res_rf, name = "boop")
    
  st_log_1_newname <-
    stacks() %>%
    add_candidates(log_res_rf, name = "boop")
  
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
      add_candidates(reg_res_sp, "reg_res_svm"),
    "has the same name"
  )
  
  expect_error(
    st_class_1 %>% 
      add_candidates(class_res_nn, "class_res_rf"),
    "has the same name"
  )
  
  expect_error(
    st_log_1 %>% 
      add_candidates(log_res_nn, "log_res_rf"),
    "has the same name"
  )
})
