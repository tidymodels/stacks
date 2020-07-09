# This script implements some basic model definition for use in examples and tests.

# setup: packages, data, resample, basic recipe ------------------------
devtools::load_all()
library(tune)
library(rsample)
library(parsnip)
library(workflows)
library(recipes)
library(yardstick)
data("penguins", package = "palmerpenguins")

penguins <- penguins[!is.na(penguins$sex),]

set.seed(1)

ctrl_grid <- 
  control_grid(
    save_pred = TRUE,
    save_workflow = TRUE
  )

ctrl_res <- 
  control_resamples(
    save_pred = TRUE,
    save_workflow = TRUE
  )

penguins_split <- initial_split(penguins)
penguins_train <- training(penguins_split)
penguins_test  <- testing(penguins_split)

folds <- vfold_cv(penguins_train, v = 5)

penguins_reg_rec <- 
  recipe(body_mass_g ~ ., data = penguins_train) %>%
  step_dummy(recipes::all_nominal()) %>%
  step_zv(all_predictors())

metric <- metric_set(rmse)

# linear regression ---------------------------------------
lin_reg_spec <-
  linear_reg() %>%
  set_engine("lm")

reg_wf_lr <- 
  workflow() %>%
  add_model(lin_reg_spec) %>%
  add_recipe(penguins_reg_rec)

reg_res_lr <- 
  fit_resamples(
    object = reg_wf_lr,
    resamples = folds,
    metrics = metric,
    control = ctrl_res
  )

# SVM regression ----------------------------------
svm_spec <- 
  svm_rbf(
    cost = tune(), 
    rbf_sigma = tune()
  ) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

reg_wf_svm <- 
  workflow() %>%
  add_model(svm_spec) %>%
  add_recipe(penguins_reg_rec)

reg_res_svm <- 
  tune_grid(
    object = reg_wf_svm,
    resamples = folds, 
    grid = 5,
    control = ctrl_grid
  )

# spline regression ---------------------------------------
spline_rec <- 
  penguins_reg_rec %>%
  step_ns(bill_length_mm, deg_free = tune::tune("length")) %>%
  step_ns(bill_depth_mm, deg_free = tune::tune("depth"))

reg_wf_sp <- 
  workflow() %>%
  add_model(lin_reg_spec) %>%
  add_recipe(spline_rec)

reg_res_sp <- 
  tune_grid(
    object = reg_wf_sp,
    resamples = folds,
    metrics = metric,
    control = ctrl_grid
  )

# classification - preliminaries -----------------------------------
penguins_class_rec <- 
  recipe(species ~ ., data = penguins_train) %>%
  step_dummy(recipes::all_nominal(), -species) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric())

# random forest classification --------------------------------------
rand_forest_spec <- 
  rand_forest(
    mtry = tune(),
    trees = 500,
    min_n = tune()
  ) %>%
  set_mode("classification") %>%
  set_engine("ranger")

class_wf_rf <-
  workflow() %>%
  add_recipe(penguins_class_rec) %>%
  add_model(rand_forest_spec)

class_res_rf <- 
  tune_grid(
    object = class_wf_rf, 
    resamples = folds, 
    grid = 10,
    control = ctrl_grid
  )

# neural network classification -------------------------------------
nnet_spec <-
  mlp(epochs = 100, hidden_units = 5, dropout = 0.1) %>%
  set_mode("classification") %>%
  set_engine("keras", verbose = 0)

class_wf_nn <- 
  workflow() %>%
  add_recipe(penguins_class_rec) %>%
  add_model(nnet_spec)

class_res_nn <-
  fit_resamples(
    object = class_wf_nn, 
    resamples = folds, 
    control = ctrl_res
  )

# binary classification --------------------------------
penguins_2_class_rec <- 
  recipe(sex ~ ., data = penguins_train) %>%
  step_dummy(recipes::all_nominal(), -sex) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric())

rand_forest_spec_2 <- 
  rand_forest(
    mtry = tune(),
    trees = 500,
    min_n = tune()
  ) %>%
  set_mode("classification") %>%
  set_engine("ranger")

log_wf_rf <-
  workflow() %>%
  add_recipe(penguins_2_class_rec) %>%
  add_model(rand_forest_spec_2)

log_res_rf <- 
  tune_grid(
    object = log_wf_rf, 
    resamples = folds, 
    grid = 10,
    control = ctrl_grid
  )

nnet_spec_2 <-
  mlp(epochs = 100, hidden_units = 5, dropout = 0.1) %>%
  set_mode("classification") %>%
  set_engine("keras", verbose = 0)

log_wf_nn <- 
  workflow() %>%
  add_recipe(penguins_2_class_rec) %>%
  add_model(nnet_spec_2)

log_res_nn <-
  fit_resamples(
    object = log_wf_nn, 
    resamples = folds, 
    control = ctrl_res
  )

# save workflows and resamples -------------------------------------
usethis::use_data(reg_res_lr, overwrite = TRUE)
usethis::use_data(reg_res_svm, overwrite = TRUE)
usethis::use_data(reg_res_sp, overwrite = TRUE)
usethis::use_data(class_res_rf, overwrite = TRUE)
usethis::use_data(class_res_nn, overwrite = TRUE)
usethis::use_data(log_res_rf, overwrite = TRUE)
usethis::use_data(log_res_nn, overwrite = TRUE)
