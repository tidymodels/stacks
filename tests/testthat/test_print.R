if ((!on_cran()) || interactive()) {
  if (on_github()) {
    load(paste0(Sys.getenv("GITHUB_WORKSPACE"), "/tests/testthat/helper_data.Rda"))
  } else {
    load(test_path("helper_data.Rda"))
  }
}

skip_if_not_installed("modeldata")
library(modeldata)

skip_if_not_installed("ranger")
library(ranger)

skip_if_not_installed("nnet")
library(nnet)

test_that("data stack printing works", {
  skip_on_cran()
  
  verify_output(
    test_path("out/data_stack_init.txt"),
    {stacks()}
  )
  
  verify_output(
    test_path("out/data_stack_reg.txt"),
    {st_reg_1}
  )
  
  verify_output(
    test_path("out/data_stack_class.txt"),
    {st_class_1}
  )
  
  verify_output(
    test_path("out/data_stack_log.txt"),
    {st_log_1}
  )
})

test_that("model stack printing works", {
  skip_on_cran()
  
  verify_output(
    test_path("out/model_stack_reg.txt"),
    {st_reg_1_}
  )
  
  verify_output(
    test_path("out/model_stack_class.txt"),
    {st_class_1_}
  )
  
  verify_output(
    test_path("out/model_stack_log.txt"),
    {st_log_1_}
  )
  
  verify_output(
    test_path("out/model_stack_reg_fit.txt"),
    {st_reg_1__}
  )
  
  verify_output(
    test_path("out/model_stack_class_fit.txt"),
    {st_class_1__}
  )
  
  verify_output(
    test_path("out/model_stack_log_fit.txt"),
    {st_log_1__}
  )
})
