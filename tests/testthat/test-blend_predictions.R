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

skip_if_not_installed("yardstick")

test_that("blend_predictions works", {
  skip_on_cran()

  expect_s3_class(st_reg_1 |> blend_predictions(), "model_stack")
  expect_s3_class(st_class_1 |> blend_predictions(), "model_stack")
  expect_s3_class(st_log_1 |> blend_predictions(), "model_stack")

  expect_null(st_reg_1_[["member_fits"]])
  expect_null(st_class_1_[["member_fits"]])
  expect_null(st_log_1_[["member_fits"]])
})

test_that("penalty arguments work correctly", {
  skip_on_cran()

  st_1 <- st_reg_1 |> blend_predictions(penalty = 10^-2)
  st_2 <- st_reg_1 |> blend_predictions(penalty = 10^-3)
  st_3 <- st_reg_1 |> blend_predictions(penalty = 10^-2, mixture = .5)

  expect_s3_class(st_1, "model_stack")
  expect_s3_class(st_2, "model_stack")
  expect_s3_class(st_3, "model_stack")

  expect_equal(class(all.equal(st_1, st_2)), "character")
  expect_equal(class(all.equal(st_1, st_3)), "character")
})

test_that("blend_predictions can handle many resample types", {
  skip_on_cran()

  expect_s3_class(
    stacks() |> add_candidates(reg_res_svm_2) |> blend_predictions(),
    "model_stack"
  )

  expect_s3_class(
    stacks() |> add_candidates(reg_res_svm_3) |> blend_predictions(),
    "model_stack"
  )

  expect_s3_class(
    stacks() |> add_candidates(reg_res_svm_4) |> blend_predictions(),
    "model_stack"
  )

  expect_s3_class(
    stacks() |> add_candidates(reg_res_svm_5) |> blend_predictions(),
    "model_stack"
  )
})

test_that("blend_predictions errors informatively with bad arguments", {
  skip_on_cran()
  skip_if_not_installed("yardstick")

  expect_snapshot(error = TRUE, res <- st_reg_1__ |> blend_predictions())

  expect_snapshot(
    error = TRUE,
    res <- st_reg_1 |> blend_predictions(non_negative = "Yup")
  )

  expect_snapshot(
    error = TRUE,
    res <- st_reg_1 |> blend_predictions(metric = "Yup")
  )

  expect_snapshot(
    error = TRUE,
    res <- st_reg_1 |> blend_predictions(metric = yardstick::accuracy)
  )

  expect_snapshot(error = TRUE, res <- stacks() |> blend_predictions())

  expect_snapshot(
    error = TRUE,
    res <- stacks() |> add_candidates(reg_res_lr) |> blend_predictions()
  )

  expect_snapshot(
    error = TRUE,
    res <- stacks() |> add_candidates(class_res_nn) |> blend_predictions()
  )

  expect_snapshot(
    error = TRUE,
    res <- stacks() |> add_candidates(log_res_nn) |> blend_predictions()
  )

  expect_snapshot(
    error = TRUE,
    res <- st_reg_1 |> blend_predictions(penalty = -1)
  )

  expect_snapshot(
    error = TRUE,
    res <- st_reg_1 |> blend_predictions(mixture = -1)
  )

  expect_snapshot(
    error = TRUE,
    res <- st_reg_1 |> blend_predictions(penalty = "lots")
  )

  expect_snapshot(
    error = TRUE,
    res <- st_reg_1 |> blend_predictions(penalty = tibble::tibble())
  )

  expect_snapshot(
    error = TRUE,
    res <- st_reg_1 |> blend_predictions(numeric(0))
  )
})

test_that("blend_predictions is sensitive to the non_negative argument", {
  skip_on_cran()

  neg <- st_reg_1 |> blend_predictions(non_negative = FALSE)

  expect_true(
    nrow(.get_glmn_coefs(st_reg_1_$coefs$fit)) <=
      nrow(.get_glmn_coefs(neg$coefs$fit))
  )
})

test_that("blend_predictions is sensitive to the metric argument", {
  skip_if_not_installed("yardstick")
  library(yardstick)
  skip_on_cran()

  metric_1 <- st_reg_1 |> blend_predictions(metric = metric_set(rmse))
  metric_2 <- st_reg_1 |> blend_predictions(metric = metric_set(rmse, mase))

  expect_true(
    nrow(metric_1$metrics) <= nrow(metric_2$metrics)
  )
})

test_that("blend_predictions is sensitive to the times argument", {
  skip_on_cran()

  times_5 <- st_reg_1 |> blend_predictions(times = 5)

  expect_equal(length(times_5[["splits"]][["splits"]]), 5)
})

test_that("process_data_stack works", {
  skip_on_cran()

  expect_equal(
    process_data_stack(data.frame(a = 1:5)),
    tibble::as_tibble(data.frame(a = 1:5))
  )

  expect_message(
    process_data_stack(data.frame(a = c(1:5, NA))),
    "1 of the 6 rows in the data stack has"
  )

  expect_message(
    process_data_stack(data.frame(a = c(1:4, NA, NA))),
    "2 of the 6 rows in the data stack have"
  )

  expect_snapshot(error = TRUE, process_data_stack(data.frame(a = rep(NA, 5))))
})

test_that("coef environments are small (#116)", {
  skip_on_cran()

  expect_equal(
    st_reg_1_$coefs$spec$eng_arg$lower.limits,
    rlang::new_quosure(0, env = rlang::empty_env())
  )

  expect_true(all(
    names(attr(st_reg_1_$coefs$preproc$terms, ".Environment")) %in%
      c("data", "weights")
  ))
})
