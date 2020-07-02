# This script implements some basic model definition for use in examples and tests.

# setup: packages, data, resample, basic recipe ------------------------
devtools::load_all()
library(tidymodels)
data("penguins", package = "palmerpenguins")

penguins <- penguins[!is.na(penguins$sex),]

set.seed(1)

ctrl_grid <- control_grid(save_pred = TRUE)
ctrl_res <- control_grid(save_pred = TRUE)

penguins_split <- initial_split(penguins)
penguins_train <- training(penguins_split)
penguins_test  <- testing(penguins_split)

folds <- vfold_cv(penguins_train, v = 5)

penguins_reg_rec <- 
  recipe(body_mass_g ~ ., data = penguins_train) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors())

metric <- metric_set(rmse)

# linear regression ---------------------------------------
lin_reg_spec <-
  linear_reg() %>%
  set_engine("lm")

lin_reg_wf_ <- 
  workflow() %>%
  add_model(lin_reg_spec) %>%
  add_recipe(penguins_reg_rec)

lin_reg_res_ <- 
  fit_resamples(
    object = lin_reg_spec,
    preprocessor = penguins_reg_rec,
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

svm_wf_ <- 
  workflow() %>%
  add_model(svm_spec) %>%
  add_recipe(penguins_reg_rec)

svm_res_ <- 
  tune_grid(
    object = svm_spec, 
    preprocessor = penguins_reg_rec, 
    resamples = folds, 
    grid = 5,
    control = ctrl_grid
  )

# spline regression ---------------------------------------
spline_rec <- 
  penguins_reg_rec %>%
  step_ns(bill_length_mm, deg_free = tune::tune("length")) %>%
  step_ns(bill_depth_mm, deg_free = tune::tune("depth"))

spline_wf_ <- 
  workflow() %>%
  add_model(lin_reg_spec) %>%
  add_recipe(spline_rec)

spline_res_ <- 
  tune_grid(
    object = lin_reg_spec,
    preprocessor = spline_rec,
    resamples = folds,
    metrics = metric,
    control = ctrl_grid
  )

# classification - preliminaries -----------------------------------
penguins_class_rec <- 
  recipe(species ~ ., data = penguins_train) %>%
  step_dummy(all_nominal(), -species) %>%
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

rand_forest_wf_ <-
  workflow() %>%
  add_recipe(penguins_class_rec) %>%
  add_model(rand_forest_spec)

rand_forest_res_ <- 
  tune_grid(
    object = rand_forest_spec, 
    preprocessor = penguins_class_rec, 
    resamples = folds, 
    grid = 10,
    control = ctrl_grid
  )


# neural network classification -------------------------------------
nnet_spec <-
  mlp(epochs = 100, hidden_units = 5, dropout = 0.1) %>%
  set_mode("classification") %>%
  set_engine("keras", verbose = 0)

nnet_wf_ <- 
  workflow() %>%
  add_recipe(penguins_class_rec) %>%
  add_model(nnet_spec)

nnet_res_ <-
  fit_resamples(
    object = nnet_spec, 
    preprocessor = penguins_class_rec, 
    resamples = folds, 
    control = ctrl_res
  )

# binary classification - just for testing for now -------------------
penguins_2_class_rec <- 
  recipe(sex ~ ., data = penguins_train) %>%
  step_dummy(all_nominal(), -sex) %>%
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

rand_forest_wf_2_ <-
  workflow() %>%
  add_recipe(penguins_2_class_rec) %>%
  add_model(rand_forest_spec_2)

rand_forest_res_2_ <- 
  tune_grid(
    object = rand_forest_spec_2, 
    preprocessor = penguins_2_class_rec, 
    resamples = folds, 
    grid = 10,
    control = ctrl_grid
  )

nnet_spec_2 <-
  mlp(epochs = 100, hidden_units = 5, dropout = 0.1) %>%
  set_mode("classification") %>%
  set_engine("keras", verbose = 0)

nnet_wf_2_ <- 
  workflow() %>%
  add_recipe(penguins_2_class_rec) %>%
  add_model(nnet_spec_2)

nnet_res_2_ <-
  fit_resamples(
    object = nnet_spec_2, 
    preprocessor = penguins_2_class_rec, 
    resamples = folds, 
    control = ctrl_res
  )

# save workflows and resamples -------------------------------------
# usethis::use_data(lin_reg_wf_, overwrite = TRUE)
# usethis::use_data(svm_wf_, overwrite = TRUE)
# usethis::use_data(spline_wf_, overwrite = TRUE)
# usethis::use_data(rand_forest_wf_, overwrite = TRUE)
# usethis::use_data(nnet_wf_, overwrite = TRUE)
# 
# usethis::use_data(lin_reg_res_, overwrite = TRUE)
# usethis::use_data(svm_res_, overwrite = TRUE)
# usethis::use_data(spline_res_, overwrite = TRUE)
# usethis::use_data(rand_forest_res_, overwrite = TRUE)
# usethis::use_data(nnet_res_, overwrite = TRUE)
