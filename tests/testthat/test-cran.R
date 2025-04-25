# many of the tests for core verbs take a significant amount of time to run.
# skip most all of them on cran in favor of a minimal test that will at least
# flag breakages, even if uninformatively.

skip_if_not_installed("modeldata")
library(modeldata)

skip_if_not_installed("ranger")
library(ranger)

skip_if_not_installed("kernlab")
library(kernlab)

skip_if_not_installed("nnet")
library(nnet)

test_that("basic stacks pipeline works", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("purrr")
  skip_if_not_installed("tune")
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("rsample")
  skip_if_not_installed("glmnet")

  set.seed(1)

  dat <-
    tibble::tibble(
      x = rnorm(200),
      y = x + rnorm(200, 0, .1),
      z = runif(1) > x
    )

  lin_reg <-
    tune::tune_grid(
      workflows::workflow() |>
        workflows::add_formula(x ~ y + z) |>
        workflows::add_model(
          parsnip::linear_reg(
            penalty = tune::tune("penalty"),
            mixture = tune::tune("mixture")
          ) |>
            parsnip::set_engine("glmnet")
        ),
      rsample::vfold_cv(dat, v = 4),
      grid = 4,
      control = control_stack_grid()
    )

  st <-
    stacks() |>
    add_candidates(lin_reg) |>
    blend_predictions() |>
    fit_members()

  expect_true(model_stack_constr(st))
})
