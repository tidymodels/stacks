context("stack_add")

test_that("stack can add members", {
  expect_equal(
    st_0 %>% stack_add(svm_res_),
    st_1
  )
  
  expect_equal(
    st_1 %>% stack_add(spline_res_),
    st_2
  )
  
  expect_true(length(st_0$members) < length(st_1$members))
  expect_true(length(st_1$members) < length(st_2$members))
})

test_that("stack won't add bad members", {
  expect_error(
    st_1 %>% stack_add(svm_res_),
    "has the same object name"
  )
  
  expect_error(
    st_0 %>%
      stack_add(spline_res_) %>%
      stack_add(svm_res_new_folds_),
    "same resampling object"
  )
})
