context("print")

load(test_path("helper_data.Rda"))

test_that("data stack printing works", {
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
