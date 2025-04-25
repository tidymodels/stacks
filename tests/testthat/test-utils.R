if ((!on_cran()) || interactive()) {
  if (on_github()) {
    load(paste0(
      Sys.getenv("GITHUB_WORKSPACE"),
      "/tests/testthat/helper_data.Rda"
    ))
  } else {
    load(test_path("helper_data.Rda"))
  }
}

skip_if_not_installed("modeldata")
library(modeldata)

skip_if_not_installed("ranger")
library(ranger)

skip_if_not_installed("kernlab")
library(kernlab)

skip_if_not_installed("nnet")
library(nnet)

test_that("object types relate as expected", {
  skip_on_cran()

  atts_d <- attributes(st_reg_1)

  expect_s3_class(st_reg_1, "data_stack")
  expect_s3_class(st_reg_1_, "model_stack")

  expect_equal(atts_d$outcome, st_reg_1_[["outcome"]])
  expect_equal(atts_d$mode, st_reg_1_[["mode"]])
  expect_equal(atts_d$model_defs, st_reg_1_[["model_defs"]])
  expect_equal(atts_d$cols_map, st_reg_1_[["cols_map"]])
  expect_equal(atts_d$model_metrics, st_reg_1_[["model_metrics"]])
  expect_equal(atts_d$train, st_reg_1_[["train"]])

  expect_equal(tibble::as_tibble(st_reg_1), st_reg_1_[["data_stack"]])
})

test_that("control_* functions work", {
  skip_on_cran()

  ctrl_grid <- control_stack_grid()
  ctrl_bayes <- control_stack_bayes()
  ctrl_res <- control_stack_resamples()

  expect_true(ctrl_grid$save_pred)
  expect_true(ctrl_bayes$save_pred)
  expect_true(ctrl_res$save_pred)

  expect_true(ctrl_grid$save_workflow)
  expect_true(ctrl_bayes$save_workflow)
  expect_true(ctrl_res$save_workflow)

  expect_s3_class(ctrl_grid, "control_grid")
  expect_s3_class(ctrl_bayes, "control_bayes")
  expect_s3_class(ctrl_res, "control_resamples")
})

test_that("misc. utilities work", {
  skip_on_cran()

  expect_snapshot(error = TRUE, check_inherits("howdy", "numeric"))
  expect_true(check_inherits("howdy", "character"))

  yall <- "y'all"

  expect_snapshot(
    res <- check_empty_ellipses(yall)
  )

  going <- "on"

  expect_snapshot(
    res <- check_empty_ellipses(hey = yall, what = "is", going)
  )
})
