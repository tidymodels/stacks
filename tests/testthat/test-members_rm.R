context("members_rm")

test_that("0-member stack is like model_stack()", {
  expect_true(inherits(st_0_rm, "stack"))
  expect_true(inherits(st_0_rm, "list"))
  
  expect_null(get_outcome(st_0_rm))
  
  expect_equal(st_0_rm, st_0)
})

test_that("objects from new resample can be added to 0-member stack", {
  st_1_post_rm <- st_0_rm %>%
    members_add(svm_res_new_folds_)
  
  inherits(st_1_post_rm, "stack")
})

test_that("stack won't add bad members", {
  expect_error(
    st_1 %>% members_rm(svm_res_),
    "member to remove, svm_res_\\b.*?\\bactual member"
  )
  
  expect_error(
    st_1 %>%
      members_rm("spline_res_"),
    "spline_res_\\b.*?\\bisn't a stack member"
  )
})
