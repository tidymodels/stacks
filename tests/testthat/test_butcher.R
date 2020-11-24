context("butcher")

if ((!on_cran()) || interactive()) {
  if (on_github()) {
    load("/Users/runner/work/stacks/stacks/tests/testthat/helper_data.Rda")
  } else {
    load(test_path("helper_data.Rda"))
  }
}

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
  skip_on_cran()
  
  test_axe(st_reg_1__, axe_call)
  test_axe(st_class_1__, axe_call)
  test_axe(st_log_1__, axe_call)
})

test_that("model_stack + axe_ctrl() works", {
  skip_on_cran()
  
  test_axe(st_reg_1__, axe_ctrl)
  test_axe(st_class_1__, axe_ctrl)
  test_axe(st_log_1__, axe_ctrl)
})

test_that("model_stack + axe_data() works", {
  skip_on_cran()
  
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
  skip_on_cran()
  
  test_axe(st_reg_1__, axe_env)
  test_axe(st_class_1__, axe_env)
  test_axe(st_log_1__, axe_env)
})

test_that("model_stack + axe_fitted() works", {
  skip_on_cran()
  
  test_axe(st_reg_1__, axe_fitted)
  test_axe(st_class_1__, axe_fitted)
  test_axe(st_log_1__, axe_fitted)
})

test_that("model_stack + butcher() works", {
  skip_on_cran()
  
  test_axe(st_reg_1__, butcher)
  test_axe(st_class_1__, butcher)
  test_axe(st_log_1__, axe_call)
})

test_that("butchered model stack printing works", {
  skip_on_cran()
  
  verify_output(
    test_path("out/model_stack_reg_butcher.txt"),
    {butcher(st_reg_1__)}
  )
  
  verify_output(
    test_path("out/model_stack_class_butcher.txt"),
    {butcher(st_class_1__)}
  )
  
  verify_output(
    test_path("out/model_stack_log_butcher.txt"),
    {butcher(st_log_1__)}
  )
})
