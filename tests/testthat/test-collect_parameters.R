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

test_that("collect_parameters dispatch works", {
  skip_on_cran()

  expect_snapshot(error = TRUE, 1 |> collect_parameters())

  expect_snapshot(error = TRUE, mtcars |> collect_parameters())
})

test_that("collect_parameters errors informatively with bad arguments", {
  skip_on_cran()

  expect_snapshot(
    error = TRUE,
    st_reg_1 |> collect_parameters("the first one")
  )

  expect_snapshot(error = TRUE, stacks() |> collect_parameters("all of them"))
})

test_that("collect_parameters on a data stack works (regression)", {
  skip_on_cran()

  res <- collect_parameters(st_reg_1, "reg_res_svm")
  res2 <- collect_parameters(st_reg_2, "reg_res_sp")
  res3 <-
    collect_parameters(
      stacks() |> add_candidates(reg_res_lr, name = "lr"),
      "lr"
    )

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res2, "tbl_df")
  expect_s3_class(res3, "tbl_df")

  expect_equal(ncol(res), 3)
  expect_equal(nrow(res), 5)

  expect_equal(ncol(res2), 2)
  expect_equal(nrow(res2), 10)

  expect_equal(ncol(res3), 1)
  expect_equal(nrow(res3), 1)
})

test_that("collect_parameters on a model stack works (regression)", {
  skip_on_cran()

  res <- collect_parameters(st_reg_1_, "reg_res_svm")
  res2 <- collect_parameters(st_reg_2 |> blend_predictions(), "reg_res_sp")

  expect_s3_class(res, "tbl_df")

  expect_equal(ncol(res), 4)
  expect_equal(nrow(res), 5)

  expect_equal(ncol(res2), 3)
  expect_equal(nrow(res2), 10)

  expect_true(
    all(
      c(
        "member",
        parsnip::extract_parameter_set_dials(
          st_reg_1_$model_defs$reg_res_svm
        ) |>
          dplyr::pull(id),
        "coef"
      ) %in%
        colnames(res)
    )
  )
})

# collecting parameters on a classification stack is a bit
# trickier, so test separately
test_that("collect_parameters works (classification)", {
  skip_on_cran()

  res <- collect_parameters(st_class_1, "class_res_rf")
  res2 <- collect_parameters(st_class_1 |> blend_predictions(), "class_res_rf")

  expect_s3_class(res, "tbl_df")
  expect_s3_class(res2, "tbl_df")

  expect_equal(ncol(res), 3)
  expect_equal(nrow(res), 10)

  expect_equal(ncol(res2), 6)
  expect_equal(nrow(res2), 57)
})
