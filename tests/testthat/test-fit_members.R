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

test_that("basic fit_members works", {
  skip_on_cran()

  expect_silent(
    st_reg_1 |>
      add_candidates(reg_res_lr) |>
      blend_predictions() |>
      fit_members()
  )

  expect_silent(
    st_class_1_ |> fit_members()
  )

  expect_silent(
    st_log_1_ |> fit_members()
  )

  expect_message(
    st_reg_bad_names <- stacks() |>
      add_candidates(reg_res_svm, name = "name with spaces") |>
      blend_predictions() |>
      fit_members()
  )

  expect_message(
    st_class_bad_names <- stacks() |>
      add_candidates(class_res_rf, name = "name with spaces") |>
      blend_predictions() |>
      fit_members()
  )

  # This is functionality that modeltime depends on.
  # Drop a note in #2 if this changes. :-)
  expect_false(!is.null(st_reg_1_[["member_fits"]]))
  expect_false(is.null(st_reg_1__[["member_fits"]]))

  expect_false(!is.null(st_class_1_[["member_fits"]]))
  expect_false(is.null(st_class_1__[["member_fits"]]))

  expect_false(!is.null(st_log_1_[["member_fits"]]))
  expect_false(is.null(st_log_1__[["member_fits"]]))

  expect_s3_class(st_reg_bad_names, "linear_stack")
  expect_s3_class(st_class_bad_names, "linear_stack")
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

  expect_snapshot(error = TRUE, st_reg_1 |> fit_members(), )

  expect_snapshot(error = TRUE, "howdy" |> fit_members())

  expect_snapshot(
    out <- st_reg_1__ |> fit_members()
  )
})

test_that("fit_members checks for required packages", {
  skip_on_cran()

  # check pluralization of error
  expect_snapshot(error = TRUE, error_needs_install(letters[1], rep(FALSE, 1)))
  expect_snapshot(
    error = TRUE,
    error_needs_install(letters[1:2], rep(FALSE, 2))
  )
  expect_snapshot(
    error = TRUE,
    error_needs_install(letters[1:3], rep(FALSE, 3))
  )

  # loads dependency when it's installed but not loaded
  unloadNamespace("kernlab")

  expect_s3_class(
    st_reg_1_ |>
      fit_members(),
    "model_stack"
  )

  expect_true(isNamespaceLoaded("kernlab"))

  # errors informatively when it's not installed
  testthat::local_mocked_bindings(is_installed_ = function(x) {
    FALSE
  })

  expect_snapshot(error = TRUE, st_reg_1_ |> fit_members())
})
