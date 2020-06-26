context("stack_resamples")

test_that("stack can add members", {
  expect_equal(
    st_0 %>% stack_resamples(svm_res_),
    st_1
  )
  
  expect_equal(
    st_1 %>% stack_resamples(spline_res_),
    st_2
  )
})

test_that("stack won't add bad members", {
  expect_error(
    st_1 %>% stack_resamples(svm_res_),
    "has the same object name 'svm_res_'"
  )

  expect_error(
    st_0 %>%
      stack_resamples(spline_res_) %>%
      stack_resamples(svm_res_new_folds_),
    "same resampling object"
  )

  svm_res_renamed <- svm_res_

  expect_error(
    st_1 %>% stack_resamples(svm_res_renamed),
    "new member 'svm_res_renamed' is the same as the existing"
  )
})
