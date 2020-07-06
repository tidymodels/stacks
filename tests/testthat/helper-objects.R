# Some objects for use throughout unit tests.
context("helpers")

# Stacks
# ------------------------------------------------------------------------
st_0 <- stacks()

st_1 <- stacks() %>%
  add_candidates(reg_res_svm)

st_2 <- stacks() %>%
  add_candidates(reg_res_svm) %>%
  add_candidates(reg_res_sp)
  
# Resampling Objects
# ------------------------------------------------------------------------

# generate a resampling object with a slightly different set of folds
set.seed(2)
library(palmerpenguins)
data(penguins)
penguins <- penguins[!is.na(penguins$sex),]
penguins_split <- rsample::initial_split(penguins)
penguins_train <- rsample::training(penguins_split)
folds_ <- rsample::vfold_cv(penguins_train, v = 3)

penguins_reg_rec <- 
  recipes::recipe(body_mass_g ~ ., data = penguins_train) %>%
  recipes::step_dummy(recipes::all_nominal()) %>%
  recipes::step_zv(recipes::all_predictors())

svm_spec <- 
  parsnip::svm_rbf(
    cost = tune::tune(), 
    rbf_sigma = tune::tune()
  ) %>%
  parsnip::set_engine("kernlab") %>%
  parsnip::set_mode("regression")

reg_wf_svm <- 
  workflows::workflow() %>%
  workflows::add_model(svm_spec) %>%
  workflows::add_recipe(penguins_reg_rec)

reg_res_svm_new_folds <- 
  tune::tune_grid(
    object = reg_wf_svm,
    resamples = folds_, 
    grid = 5,
    control = control_stack_grid()
  )
