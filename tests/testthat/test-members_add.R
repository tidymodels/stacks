context("members_add")

test_that("stack can add members", {
  expect_equal(
    st_0 %>% members_add(svm_res_),
    st_1
  )
  
  expect_equal(
    st_1 %>% members_add(spline_res_),
    st_2
  )
})

test_that("stack won't add bad members", {
  expect_error(
    st_1 %>% members_add(svm_res_),
    "has the same object name"
  )
  
  expect_error(
    st_0 %>%
      members_add(spline_res_) %>%
      members_add(svm_res_new_folds_),
    "same resampling object"
  )
  
  svm_res_renamed <- svm_res_
  
  expect_error(
    st_1 %>% members_add(svm_res_renamed),
    "new member 'svm_res_renamed' is the same as the existing"
  )
})
