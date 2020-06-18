context("stack_eval")

st_1_eval <- 
  st_1 %>% 
  stack_eval(mtcars)

test_that("stack can be evaluated", {
  expect_true(is_evaluated(st_1_eval))
})

test_that("warnings/errors after evaluation", {
  expect_warning(
    st_1_eval %>% stack_rm("svm_res_"),
    "with the member 'svm_res_' removed."
  )
  expect_warning(
    st_1_eval %>% stack_add(spline_res_),
    "with the new member 'spline_res_' added."
  )
})

test_that("stack can be unevaluated", {
  expect_equal(
    st_1_eval %>% stack_uneval(),
    st_1
  )
  expect_equal(
    st_1_eval %>% stack_uneval() %>% stack_rm("svm_res_"),
    stack_init()
  )
})