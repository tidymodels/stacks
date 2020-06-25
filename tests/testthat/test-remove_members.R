context("remove_members")

test_that("0-member stack is like pancakes()", {
  expect_true(inherits(st_0_rm, "stack"))
  expect_true(inherits(st_0_rm, "tbl_df"))
  
  expect_null(get_outcome(st_0_rm))
  
  expect_equal(st_0_rm, st_0)
})

test_that("objects from new resample can be added to 0-member stack", {
  st_1_post_rm <- st_0_rm %>%
    add_members(svm_res_new_folds_)
  
  expect_true(inherits(st_1_post_rm, "stack"))
})

test_that("stack won't add bad members", {
  expect_error(
    st_1 %>% remove_members(svm_res_),
    "definition to remove, svm_res_\\b.*?\\bactual model definition"
  )
  
  expect_error(
    st_1 %>%
      remove_members("spline_res_"),
    "spline_res_\\b.*?\\bin the stack"
  )
})
