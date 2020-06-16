# This script implements some basic models for use in examples and tests.
# Results are stored in R/sysdata.rda, which is loaded on package load.

# setup: resample and basic recipe ---------------------------------------

library(tidymodels)

set.seed(1)

folds_ <- rsample::vfold_cv(mtcars, v = 3)

car_rec_ <- 
  recipes::recipe(mpg ~ ., data = mtcars) %>%
  recipes::step_normalize(recipes::all_predictors())

# support vector machine -------------------------------------------------

svm_mod_ <- 
  parsnip::svm_rbf(
    cost = tune::tune(), 
    rbf_sigma = tune::tune()
  ) %>%
  parsnip::set_engine("kernlab") %>%
  parsnip::set_mode("regression")

set.seed(1)

svm_res_ <- 
  tune::tune_grid(
    object = svm_mod_, 
    preprocessor = car_rec_, 
    resamples = folds_, 
    grid = 5
  )

# spline regression ------------------------------------------------------

spline_rec_ <-
  recipes::recipe(mpg ~ ., data = mtcars) %>%
  recipes::step_ns(disp, deg_free = tune::tune("disp")) %>%
  recipes::step_ns(wt, deg_free = tune::tune("wt"))

lin_mod_ <-
  parsnip::linear_reg() %>%
  parsnip::set_engine("lm")

spline_grid_ <- expand.grid(disp = c(2, 4, 6), wt = c(2, 4, 6))

spline_res_ <- 
  tune_grid(
    object = lin_mod_,
    preprocessor = spline_rec_, 
    resamples = folds_, 
    grid = spline_grid_
  )

# save the objects to sysdata
save.image("R/sysdata.rda")

