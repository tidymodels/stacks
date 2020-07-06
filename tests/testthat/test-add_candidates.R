context("add_candidates")

test_that("stack can add members", {
  expect_equal(
    st_0 %>% add_candidates(reg_res_svm),
    st_1
  )
  
  expect_equal(
    st_1 %>% add_candidates(reg_res_sp),
    st_2
  )
})

test_that("stack won't add bad members", {
  expect_error(
    st_1 %>% add_candidates(reg_res_svm),
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
    st_1 %>% add_candidates(reg_res_svm_renamed),
    "new member 'reg_res_svm_renamed' is the same as the existing"
  )
})
