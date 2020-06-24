# Some objects for use throughout unit tests.
context("helpers")

# Stacks
# ------------------------------------------------------------------------
st_0 <- model_stack()

st_1 <- model_stack() %>%
  members_add(svm_res_)

st_0_rm <- st_1 %>%
  members_rm("svm_res_")

st_2 <- model_stack() %>%
  members_add(svm_res_) %>%
  members_add(spline_res_)

st_1_rm <- st_2 %>%
  members_rm("spline_res_")
  
# Resampling Objects
# ------------------------------------------------------------------------

# generate a resampling object with a slightly different set of folds
set.seed(2)

folds_ <- rsample::vfold_cv(mtcars, v = 3)

ctrl <- control_grid(save_pred = TRUE)

car_rec_ <- 
  recipes::recipe(mpg ~ ., data = mtcars) %>%
  recipes::step_normalize(recipes::all_predictors())

svm_mod_ <- 
  parsnip::svm_rbf(
    cost = tune::tune(), 
    rbf_sigma = tune::tune()
  ) %>%
  parsnip::set_engine("kernlab") %>%
  parsnip::set_mode("regression")

set.seed(2)

svm_res_new_folds_ <- 
  tune::tune_grid(
    object = svm_mod_, 
    preprocessor = car_rec_, 
    resamples = folds_, 
    grid = 5,
    control = ctrl
  )
