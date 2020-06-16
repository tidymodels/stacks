set.seed(6735)
folds <- vfold_cv(mtcars, v = 5)

car_rec <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_normalize(all_predictors())

# support vector machine --------------

svm_mod <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

# Use a space-filling design with 7 points
set.seed(3254)
svm_res <- tune_grid(svm_mod, car_rec, resamples = folds, grid = 7)

svm_res

collect_metrics(svm_res)

set.seed(6735)
folds <- vfold_cv(mtcars, v = 5)

# spline regression ------------------------------------

spline_rec <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_ns(disp, deg_free = tune("disp")) %>%
  step_ns(wt, deg_free = tune("wt"))

lin_mod <-
  linear_reg() %>%
  set_engine("lm")

spline_grid <- expand.grid(disp = 2:5, wt = 2:5)

spline_res <-
  tune_grid(lin_mod, spline_rec, resamples = folds, grid = spline_grid)

collect_metrics(spline_res)

# stacking ---------------------------------

stack_predictions(data = mtcars, 
                  svm = svm_res,
                  spline = spline_res,
                  n = c(3, 1))


resamples <- list(svm = svm_res,
                  spline = spline_res)


stack_predictions(
  ames_train,
  glmnet = glmnet_res,
  mars = mars_res,
  n = c(5, 1)
)
