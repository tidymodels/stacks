library(parsnip)

skip_if_not_installed("modeldata")
library(modeldata)

skip_if_not_installed("ranger")
library(ranger)

skip_if_not_installed("kernlab")
library(kernlab)

skip_if_not_installed("nnet")
library(nnet)

## -----------------------------------------------------------------------------

data("penguins", package = "modeldata")

penguins <-
  penguins |>
  na.omit() |>
  dplyr::select(
    bill_length_mm,
    bill_depth_mm,
    flipper_length_mm,
    body_mass_g,
    species
  )

data(two_class_dat, package = "modeldata")

## -----------------------------------------------------------------------------

test_that("linear regression", {
  reg_model <-
    linear_reg(penalty = .1) |>
    set_engine("glmnet") |>
    set_mode("regression") |>
    fit(
      body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
      data = penguins
    )

  reg_preds_parsnip <- predict(reg_model, penguins)

  reg_eqns <- prediction_eqn(reg_model)

  reg_preds_eqns <- stack_predict(reg_eqns, data = penguins)

  expect_equal(reg_preds_parsnip, reg_preds_eqns)
})


## -----------------------------------------------------------------------------

test_that("logistic regression", {
  bin_model <-
    logistic_reg(penalty = .1) |>
    set_engine("glmnet") |>
    set_mode("classification") |>
    fit(Class ~ ., data = two_class_dat)

  bin_cls_eqns <- prediction_eqn(bin_model, type = "class")
  bin_cls_parsnip <- predict(bin_model, two_class_dat, type = "class")
  bin_cls_eqns <- stack_predict(bin_cls_eqns, data = two_class_dat)
  expect_equal(bin_cls_parsnip, bin_cls_eqns)

  bin_prob_eqns <- prediction_eqn(bin_model, type = "prob")
  bin_prob_parsnip <- predict(bin_model, two_class_dat, type = "prob")
  bin_prob_eqns <- stack_predict(bin_prob_eqns, data = two_class_dat)
  expect_equal(bin_prob_parsnip, bin_prob_eqns)
})


## -----------------------------------------------------------------------------

test_that("multiclass regression", {
  mltn_model <-
    multinom_reg(penalty = .1) |>
    set_engine("glmnet") |>
    set_mode("classification") |>
    fit(species ~ ., data = penguins)

  mltn_cls_eqns <- prediction_eqn(mltn_model, type = "class")
  mltn_cls_parsnip <- predict(mltn_model, penguins, type = "class")
  mltn_cls_eqns <- stack_predict(mltn_cls_eqns, data = penguins)
  expect_equal(mltn_cls_parsnip, mltn_cls_eqns)

  mltn_prob_eqns <- prediction_eqn(mltn_model, type = "prob")
  mltn_prob_parsnip <- predict(mltn_model, penguins, type = "prob")
  mltn_prob_eqns <- stack_predict(mltn_prob_eqns, data = penguins)
  expect_equal(mltn_prob_parsnip, mltn_prob_eqns)
})
