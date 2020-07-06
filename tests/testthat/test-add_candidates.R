context("add_candidates")

test_that("stack can add candidates (regression)", {
  expect_equal(
    st_0 %>% add_candidates(reg_res_svm),
    st_reg_1
  )
  
  expect_equal(
    st_reg_1 %>% add_candidates(reg_res_sp),
    st_reg_2
  )
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
})

test_that("stack won't add bad members", {
  expect_error(
    st_reg_1 %>% add_candidates(reg_res_svm),
    "has the same object name 'reg_res_svm'"
  )

  expect_error(
    st_0 %>%
      add_candidates(reg_res_sp) %>%
      add_candidates(reg_res_svm_new_folds),
    "same resampling object"
  )

  reg_res_svm_renamed <- reg_res_svm

  expect_error(
    st_reg_1 %>% add_candidates(reg_res_svm_renamed),
    "new member 'reg_res_svm_renamed' is the same as the existing"
  )
})
