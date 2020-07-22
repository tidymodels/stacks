# Load these objects when running unit tests, but not on load_all. :-)
if (! interactive() || ! any(grepl("\\bload_all\\b", as.character(sys.calls()[1])))) {

# Some objects for use throughout unit tests.

# Stacks
# ------------------------------------------------------------------------
st_0 <- stacks()

st_reg_1 <- 
  stacks() %>%
  stack_add(reg_res_svm)

st_reg_1_ <-
  st_reg_1 %>%
  stack_blend()

st_reg_1__ <-
  st_reg_1_ %>%
  stack_fit()

st_reg_2 <- 
  stacks() %>%
  stack_add(reg_res_svm) %>%
  stack_add(reg_res_sp)

st_class_1 <- 
  stacks() %>%
  stack_add(class_res_rf)

st_class_1_ <-
  st_class_1 %>%
  stack_blend()

st_class_1__ <- 
  st_class_1_ %>%
  stack_fit()

st_class_2 <- 
  st_class_1 %>%
  stack_add(class_res_nn)

st_log_1 <- 
  stacks() %>%
  stack_add(log_res_rf)

st_log_1_ <-
  st_log_1 %>%
  stack_blend()

st_log_1__ <-
  st_log_1_ %>%
  stack_fit()

st_log_2 <- 
  st_log_1 %>%
  stack_add(log_res_nn)
  
# Resampling Objects
# ------------------------------------------------------------------------

# generate a resampling object with a slightly different set of folds
set.seed(2)
library(palmerpenguins)
data(penguins)
penguins <- penguins[!is.na(penguins$sex),]
penguins_split <- rsample::initial_split(penguins)
penguins_train <- rsample::training(penguins_split)
folds2 <- rsample::vfold_cv(penguins_train, v = 3)
folds3 <- rsample::vfold_cv(penguins_train, v = 5, repeats = 2)
folds4 <- rsample::bootstraps(penguins_train)
folds5 <- rsample::mc_cv(penguins_train)

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

reg_res_svm_2 <- 
  tune::tune_grid(
    object = reg_wf_svm,
    resamples = folds2, 
    grid = 5,
    control = control_stack_grid()
  )

reg_res_svm_3 <- 
  tune::tune_grid(
    object = reg_wf_svm,
    resamples = folds3, 
    grid = 5,
    control = control_stack_grid()
  )
  
reg_res_svm_4 <- 
  tune::tune_grid(
    object = reg_wf_svm,
    resamples = folds4, 
    grid = 5,
    control = control_stack_grid()
  )

reg_res_svm_5 <- 
  tune::tune_grid(
    object = reg_wf_svm,
    resamples = folds5, 
    grid = 5,
    control = control_stack_grid()
  )
}