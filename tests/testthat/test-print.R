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

test_that("data stack printing works", {
  skip_on_cran()

  expect_snapshot(stacks())
  expect_snapshot(st_reg_1)
  expect_snapshot(st_class_1)
  expect_snapshot(st_log_1)
})

test_that("model stack printing works", {
  skip_on_cran()

  expect_snapshot(st_reg_1_)
  expect_snapshot(st_class_1_)
  expect_snapshot(st_log_1_)

  expect_snapshot(st_reg_1__)
  expect_snapshot(st_class_1__)
  expect_snapshot(st_log_1__)
})
