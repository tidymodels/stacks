context("butcher")

# Unit testing for the component members would duplicate unit testing
# from the butcher package. Since the desired functionality to test
# is actually just whether the components to axe are in the right places:
test_axe <- function(st, fxn) {
  st_axed <- fxn(st)
  
  expect_true(object.size(st) > object.size(st_axed))
  expect_true(object.size(st[["coefs"]]) > object.size(st_axed[["coefs"]]))
  expect_true(object.size(st[["member_fits"]]) > object.size(st_axed[["member_fits"]]))
}

test_that("model_stack + axe_call() works", {
  test_axe(st_reg_1__, axe_call)
  test_axe(st_class_1__, axe_call)
  test_axe(st_log_1__, axe_call)
})

test_that("model_stack + axe_ctrl() works", {
  test_axe(st_reg_1__, axe_ctrl)
  test_axe(st_class_1__, axe_ctrl)
  test_axe(st_log_1__, axe_ctrl)
})

test_that("model_stack + axe_data() works", {
  expect_identical(
    axe_data(st_reg_1__)[["train"]],
    tibble::tibble()
  )
  
  expect_identical(
    axe_data(st_class_1__)[["train"]],
    tibble::tibble()
  )
  
  expect_identical(
    axe_data(st_log_1__)[["train"]],
    tibble::tibble()
  )
})

test_that("model_stack + axe_env() works", {
  test_axe(st_reg_1__, axe_env)
  test_axe(st_class_1__, axe_env)
  test_axe(st_log_1__, axe_env)
})

test_that("model_stack + axe_fitted() works", {
  test_axe(st_reg_1__, axe_fitted)
  test_axe(st_class_1__, axe_fitted)
  test_axe(st_log_1__, axe_fitted)
})

test_that("model_stack + butcher() works", {
  test_axe(st_reg_1__, butcher)
  test_axe(st_class_1__, butcher)
  test_axe(st_log_1__, axe_call)
})

