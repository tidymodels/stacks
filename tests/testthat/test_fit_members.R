context("fit_members")

if ((!on_cran()) || interactive()) {
  if (on_github()) {
    load(paste0(Sys.getenv("GITHUB_WORKSPACE"), "/tests/testthat/helper_data.Rda"))
  } else {
    load(test_path("helper_data.Rda"))
  }
}


test_that("basic fit_members works", {
  skip_on_cran()
  
  expect_silent(
    st_reg_1 %>% add_candidates(reg_res_lr) %>% blend_predictions() %>% fit_members()
  )
  
  expect_silent(
    st_class_1_ %>% fit_members()
  )
  
  expect_silent(
    st_log_1_ %>% fit_members()
  )
  
  # This is functionality that modeltime depends on.
  # Drop a note in #2 if this changes. :-)
  expect_false(!is.null(st_reg_1_[["member_fits"]]))
  expect_false( is.null(st_reg_1__[["member_fits"]]))
  
  expect_false(!is.null(st_class_1_[["member_fits"]]))
  expect_false( is.null(st_class_1__[["member_fits"]]))
  
  expect_false(!is.null(st_log_1_[["member_fits"]]))
  expect_false( is.null(st_log_1__[["member_fits"]]))
})

test_that("fit_members leaves most model stack elements alone", {
  skip_on_cran()
  
  expect_equal(st_reg_1__[["train"]], st_reg_1_[["train"]])
  expect_equal(st_class_1__[["train"]], st_class_1_[["train"]])
  expect_equal(st_log_1__[["train"]], st_log_1_[["train"]])
  
  expect_equal(st_reg_1__[["data_stack"]], st_reg_1_[["data_stack"]])
  expect_equal(st_class_1__[["data_stack"]], st_class_1_[["data_stack"]])
  expect_equal(st_log_1__[["data_stack"]], st_log_1_[["data_stack"]])
  
  expect_equal(st_reg_1__[["outcome"]], st_reg_1_[["outcome"]])
  expect_equal(st_class_1__[["outcome"]], st_class_1_[["outcome"]])
  expect_equal(st_log_1__[["outcome"]], st_log_1_[["outcome"]])
  
  expect_equal(st_reg_1__[["coefs"]], st_reg_1_[["coefs"]])
  expect_equal(st_class_1__[["coefs"]], st_class_1_[["coefs"]])
  expect_equal(st_log_1__[["coefs"]], st_log_1_[["coefs"]])
  
  expect_equal(st_reg_1__[["mode"]], st_reg_1_[["mode"]])
  expect_equal(st_class_1__[["mode"]], st_class_1_[["mode"]])
  expect_equal(st_log_1__[["mode"]], st_log_1_[["mode"]])
  
  expect_equal(st_reg_1__[["cols_map"]], st_reg_1_[["cols_map"]])
  expect_equal(st_class_1__[["cols_map"]], st_class_1_[["cols_map"]])
  expect_equal(st_log_1__[["cols_map"]], st_log_1_[["cols_map"]])
  
  expect_equal(st_reg_1__[["model_metrics"]], st_reg_1_[["model_metrics"]])
  expect_equal(st_class_1__[["model_metrics"]], st_class_1_[["model_metrics"]])
  expect_equal(st_log_1__[["model_metrics"]], st_log_1_[["model_metrics"]])
  
  expect_equal(st_reg_1__[["model_defs"]], st_reg_1_[["model_defs"]])
  expect_equal(st_class_1__[["model_defs"]], st_class_1_[["model_defs"]])
  expect_equal(st_log_1__[["model_defs"]], st_log_1_[["model_defs"]])
  
  expect_equal(st_reg_1__[["splits"]], st_reg_1_[["splits"]])
  expect_equal(st_class_1__[["splits"]], st_class_1_[["splits"]])
  expect_equal(st_log_1__[["splits"]], st_log_1_[["splits"]])
})

test_that("fit_members errors informatively with a bad model_stack arg", {
  skip_on_cran()
  
  expect_error(
    st_reg_1 %>% fit_members(),
    "Did you forget to first evaluate the ensemble's stacking coefficients w"
  )
  
  expect_error(
    "howdy" %>% fit_members(),
    "`model_stack` needs to inherit from `model_stack`"
  )
})
