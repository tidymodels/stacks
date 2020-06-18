context("stack_eval")

test_that("stack can be evaluated", {
  st_1_eval <- 
    st_1 %>% 
    stack_eval(mtcars)
  
  expect_true(is_evaluated(st_1_eval))
})

test_that("warnings/errors after evaluation", {
  # evaluated stack objects should warn/error when
  # members are added/removed
  expect_true(TRUE)
})
