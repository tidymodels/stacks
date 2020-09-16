context("blend_predictions")

load(test_path("helper_data.Rda"))

test_that("blend_predictions works", {
  expect_true(check_inherits(st_reg_1 %>% blend_predictions(), "model_stack"))
  expect_true(check_inherits(st_class_1 %>% blend_predictions(), "model_stack"))
  expect_true(check_inherits(st_log_1 %>% blend_predictions(), "model_stack"))
  
  expect_null(st_reg_1_[["member_fits"]])
  expect_null(st_class_1_[["member_fits"]])
  expect_null(st_log_1_[["member_fits"]])
})

test_that("penalty argument works correctly", {
  expect_true(check_inherits(st_reg_1 %>% blend_predictions(10^-2), "model_stack"))
  expect_equal(
    class(all.equal(
      st_reg_1 %>% blend_predictions(10^-2),
      st_reg_1 %>% blend_predictions(10^-3)
    )), 
    "character"
  )
})

test_that("blend_predictions can handle many resample types", {
  expect_true(
    check_inherits(
      stacks() %>% add_candidates(reg_res_svm_2) %>% blend_predictions(), 
      "model_stack"
    )
  )
  
  expect_true(
    check_inherits(
      stacks() %>% add_candidates(reg_res_svm_3) %>% blend_predictions(), 
      "model_stack"
    )
  )
  
  expect_true(
    check_inherits(
      stacks() %>% add_candidates(reg_res_svm_4) %>% blend_predictions(), 
      "model_stack"
    )
  )
  
  expect_true(
    check_inherits(
      stacks() %>% add_candidates(reg_res_svm_5) %>% blend_predictions(), 
      "model_stack"
    )
  )
})

test_that("blend_predictions errors informatively with bad arguments", {
  expect_error(
    st_reg_1__ %>% blend_predictions(),
    "needs to inherit from `data_stack`, but its class is"
  )
  
  expect_error(
    stacks() %>% blend_predictions(),
    "the argument to `data_stack` has no candidate members"
  )
  
  expect_error(
    stacks() %>% add_candidates(reg_res_lr) %>% blend_predictions(),
    "only contains one candidate member."
  )
  
  expect_error(
    stacks() %>% add_candidates(class_res_nn) %>% blend_predictions(),
    "only contains one candidate member."
  )
  
  expect_error(
    stacks() %>% add_candidates(log_res_nn) %>% blend_predictions(),
    "only contains one candidate member."
  )
  
  expect_error(
    st_reg_1 %>% blend_predictions(penalty = -1),
    "supply only nonnegative values"
  )
  
  expect_error(
    st_reg_1 %>% blend_predictions(penalty = "lots"),
    "supplied penalty's class is `character`"
  )
  
  expect_error(
    st_reg_1 %>% blend_predictions(numeric(0)),
    "Please supply one or more penalty values."
  )
})
