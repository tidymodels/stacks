if ((!on_cran()) || interactive()) {
  if (on_github()) {
    load(paste0(Sys.getenv("GITHUB_WORKSPACE"), "/tests/testthat/helper_data.Rda"))
  } else {
    load(test_path("helper_data.Rda"))
  }
}

skip_if_not_installed("modeldata")
library(modeldata)

skip_if_not_installed("ranger")
library(ranger)

skip_if_not_installed("nnet")
library(nnet)

test_that("stack can add candidates (regression)", {
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
    "should inherit from one of"
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
  
  suppressWarnings(
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
  
  dat <-
    tibble::tibble(
      x = rnorm(200),
      y = x + rnorm(200, 0, .1),
      z = factor(sample(letters[1:3], 200, TRUE))
    )
  
  # use a metric that only relies on hard class prediction
  log_res <- 
    tune::tune_grid(
      workflows::workflow() %>%
        workflows::add_formula(z ~ x + y) %>%
        workflows::add_model(
          parsnip::multinom_reg(
            penalty = tune::tune("penalty"), 
            mixture = tune::tune("mixture")
          ) %>%
            parsnip::set_engine("glmnet")
        ),
      rsample::vfold_cv(dat, v = 4),
      grid = 4,
      control = control_stack_grid(),
      metrics = yardstick::metric_set(yardstick::accuracy)
    )
  
  expect_error(
    stacks() %>% add_candidates(log_res),
    "only metrics that rely on hard class predictions"
  )
  
  # warn when stacking may fail due to tuning failure
  set.seed(7898)
  data_folds <- rsample::vfold_cv(mtcars, v = 2)
  
  rec <- recipes::recipe(mpg ~ ., data = mtcars) %>%
    recipes::step_bs(disp, deg_free = tune())
  
  model <- parsnip::linear_reg(mode = "regression", penalty = tune::tune()) %>%
    parsnip::set_engine("glmnet")
  
  cars_grid_1 <- tibble::tibble(deg_free = 10L, penalty = -1)
  
  res_w_notes <- tune::tune_grid(
    preprocessor = rec, 
    object = model,
    resamples = data_folds, 
    grid = cars_grid_1, 
    control = tune::control_grid(extract = function(x) {1}, save_pred = TRUE, save_workflow = TRUE)
  )
  
  expect_error(
    expect_warning(
      stacks() %>%
        add_candidates(res_2),
      "argument \\`res_2\\` generated notes"
    )
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
  
  expect_message(
    st_reg_1 <- 
      stacks() %>%
      add_candidates(reg_res_svm, name = "beep bop"),
    "cannot prefix a valid column name"
  )
})
