## -----------------------------------------------------------------------------
# Simple demonstration of stacking using three models with the Ames housing data

## -----------------------------------------------------------------------------

library(tidymodels)
library(rules)
library(doMC)
library(AmesHousing)
library(ggforce)

registerDoMC(cores = 2)

## -----------------------------------------------------------------------------

ames <-
  make_ames() %>%
  # Remove quality-related predictors
  dplyr::select(-matches("Qu")) %>%
  mutate(Sale_Price = log10(Sale_Price))

set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")
ames_train <- training(data_split)
ames_test  <- testing(data_split)

set.seed(8051)
rs <- vfold_cv(ames_train, strata = Sale_Price)

# ------------------------------------------------------------------------------
# We'll use different pre-processing for different models but we can start with
# this:

basic_rec <-
  recipe(Sale_Price ~ ., data = ames_train) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors())

# Stacking will always need the out-of-sample predictions
ctrl <- control_grid(save_pred = TRUE)

metric <- metric_set(rmse)

# ------------------------------------------------------------------------------
# Fit a cubist rule-based model:

cubist_grid <- expand.grid(committees = 10 * c(1, 3, 5), neighbors = c(0, 1, 3, 
                                                                       5, 7, 9))

set.seed(7966)
cubist_res <-
  cubist_rules(committees = tune(), neighbors = tune()) %>%
  set_engine("Cubist") %>%
  tune_grid(
    Sale_Price ~ .,
    resamples = rs,
    grid = cubist_grid,
    control = ctrl,
    metrics = metric
  )

autoplot(cubist_res)

# ------------------------------------------------------------------------------
# lasso model with supplimental nonlinear terms for a few key terms:

glmnet_rec <-
  basic_rec %>%
  step_ns(Longitude, deg_free = tune("longitude_df"))%>%
  step_ns(Latitude, deg_free = tune("latitude_df"))%>%
  step_ns(Gr_Liv_Area, deg_free = tune("live_area_df")) %>%
  step_normalize(all_predictors())

glmnet_mod <-
  linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

glmnet_wflow <-
  workflow() %>%
  add_recipe(glmnet_rec) %>%
  add_model(glmnet_mod)

glmnet_param <-
  glmnet_wflow %>%
  parameters() %>%
  update(
    longitude_df = spline_degree(c(3, 50)),
    latitude_df = spline_degree(c(3, 50)),
    live_area_df = spline_degree(c(3, 50))
  )

set.seed(419)
glmnet_res <-
  glmnet_wflow %>%
  tune_grid(resamples = rs, grid = 25, control = ctrl, metrics = metric)

autoplot(glmnet_res)


# ------------------------------------------------------------------------------
# MARS models

mars_grid <- expand.grid(num_terms = seq(2, 50, by = 2), prod_degree = 1:2)

set.seed(175)
mars_res <-
  mars(num_terms = tune(), prod_degree = tune()) %>%
  set_engine("earth") %>%
  tune_grid(
    basic_rec,
    resamples = rs,
    grid = mars_grid,
    control = ctrl,
    metrics = metric
  )

autoplot(mars_res)

# ------------------------------------------------------------------------------
# Stacking requires two things for each model:
#   - A fitted workflow object for each model in the ensemble
#   - The resampling results from `fit_resamples()`, `tune_grid()`, or `tune_bayes()`
#
# This function will collate the sets of out-of-sample predictions from the
# resampling results. This will need to be extended in a few ways as well as
# adding some convenience functions to assess the diversity of the ensemble
# results.
#
# Let's assume that we will select the sub-model with the smallest RMSE. We should
# generalize that along with the ability to use all/some sub-models in the
# tuning results.

get_best_pred <- function(x, pred_name = stop("need a name")) {
  nm_list <- list(".pred")
  names(nm_list) <- pred_name
  keep_cols <- c(".row", pred_name)


  mod_name <-
  res <- select_best(x, metric = "rmse")
  pred <- collect_predictions(x, parameters = res, summarize = TRUE) %>%
    ungroup()

  pred %>% dplyr::select(!!!nm_list, .row)
}

stack_data <-
  ames_train %>%
  select(Sale_Price) %>%
  add_rowindex() %>%
  full_join(get_best_pred(cubist_res, "cubist"), by = ".row") %>%
  full_join(get_best_pred(glmnet_res, "glmnet"), by = ".row") %>%
  full_join(get_best_pred(mars_res, "mars"), by = ".row") %>%
  dplyr::select(-.row)

model_correlations <- cor(stack_data)
round(cor(stack_data), 2)
prcomp(model_correlations[-1, -1])

ggplot(stack_data, aes(x = .panel_x, y = .panel_y)) +
  geom_point(alpha = .3, size = 1) +
  geom_autodensity(alpha = .25) +
  facet_matrix(vars(everything()), layer.diag = 2) +
  theme_bw()

## -----------------------------------------------------------------------------
# Create the stacking loadings/coefficients. Given the correlations between
# model results, regularization should be used along with a non-negative
# coefficient constraint. glmnet is a good option but Bayesian methods might be
# a good choice too.

stack_model_spec <-
  linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet", lower.limits = 0)

set.seed(1872)
stack_model_res <-
  stack_model_spec %>%
  tune_grid(
    Sale_Price ~ .,
    resamples = bootstraps(stack_data),    # any resampling method would work here
    grid = tibble(penalty = 10 ^ (-6:-1)),
    metrics = metric
  )

autoplot(stack_model_res)

stack_model_fit <-
  stack_model_spec %>%
  finalize_model(tibble(penalty = 10^-3)) %>%
  fit(Sale_Price ~ ., data = stack_data)

# What are the actual coefficients for each model?
coef(stack_model_fit$fit, s = 10^-3) %>%
  as.matrix() %>%
  t() %>%
  as_tibble()

## -----------------------------------------------------------------------------
# At this point, we would want to have fitted workflows for each model with
# non-zero coefficients. These would need to be bundled into an object so that
# we have a single-point of prediction. See next section for a mock-up of some
# interface thoughts.


## -----------------------------------------------------------------------------
# API thoughts... here is my first thought about how the process might work.
# There's a lot of hand-waving here until we figure out some details.

if (FALSE) {
  # This collates the resampling results and catalogs a few things about the
  # models.
  res <-
    ens_stacked() %>%
    add_member(fit = cubist_fit, results = cubist_res) %>%
    add_member(fit = glmnet_fit, results = glmnet_res) %>%
    add_member(fit = mars_fit,   results = mars_res)

  # Notes:
  #
  # 1. Rather than using a tibble as the primary data object for 'res', we
  #    would use a list I think.
  #
  # 2. We'd need to hope that the fit is matched to the grid results. I don't know
  #   of a way to do that. Potentially, the `fit` wouldn't be needed until the end
  #   so they could leave that empty and we do the fit at the end.
  #
  # 3. Instead of `fit`, maybe we should ask for the workflow and enforce a
  #    re-fitting
  #
  # 4. Use some verb to execute the process of estimating the stacking coefficients.
  #    `evaluate`? `components` would be a good non-verb choice. Or... we could use
  #    `fit` to calculate the coefficients and `refit` to make the object that has
  #     the ensemble members within.

  # This would add the coefficients to the object.
  res <- evaluate(res, model = "glmnet", parameters, resampling)

  # More notes:
  #
  # 5. This object should be able to autoplot() the results of the tuning and
  #    allow users to set the amount of regularization and finalize. We might
  #    need a different object type here to do that part.
  #
  # 6. When we save the final coefficients, it should be a simple R expression
  #    that can be evaluated in a `dplyr::mutate()`. This will make the object
  #    smaller and also avoid having to have model objects for configurations/models
  #    that have coefficients of zero.

  res <- fit(res, training = ames_train)

  predict(res, new_data = ames_test) # `type` argument for classification models

}


## -----------------------------------------------------------------------------
# Also, a lot of people would add _all_ of the sub-model results to the ensemble
# and let the regularized model select what is best for the ensemble.  That's
# basically what h2o.ai does

get_all_pred <- function(x, pred_name = stop("need a name")) {
  nm_list <- list(".pred")
  names(nm_list) <- pred_name
  keep_cols <- c(".row", pred_name)

  params <- attributes(x)$parameters$id

  pred <-
    collect_predictions(x, summarize = TRUE) %>%
    ungroup()

  # Julia has a PR working to automatically assign a configuration ID to
  # each parameter combination. We could use that and swap the prefix to pred_name
  # https://github.com/tidymodels/tune/pull/227
  configs <-
    pred %>%
    dplyr::select(!!!params) %>%
    distinct()
  configs$.config <- recipes::names0(nrow(configs), pred_name)

  pred <-
    pred %>%
    full_join(configs, by = params) %>%
    dplyr::select(.row, .pred, .config) %>%
    tidyr::pivot_wider(id_cols = ".row", names_from = ".config", values_from = ".pred")
  pred  # we will also have to save the mapping from the parameters to .config
}

# This seems slower than it should be
stack_submodels <-
  ames_train %>%
  select(Sale_Price) %>%
  add_rowindex() %>%
  full_join(get_all_pred(cubist_res, "cubist"), by = ".row") %>%
  full_join(get_all_pred(glmnet_res, "glmnet"), by = ".row") %>%
  full_join(get_all_pred(mars_res, "mars"), by = ".row") %>%
  dplyr::select(-.row)


submodel_correlations <- cor(stack_submodels)
round(submodel_correlations, 2)
prcomp(submodel_correlations[-1, -1])

ggplot(stack_submodels, aes(x = .panel_x, y = .panel_y)) +
  geom_point(alpha = .3, size = 1) +
  geom_autodensity(alpha = .25) +
  facet_matrix(vars(everything()), layer.diag = 2) +
  theme_bw()

set.seed(1872)
stack_submodel_res <-
  stack_model_spec %>%
  tune_grid(
    Sale_Price ~ .,
    resamples = bootstraps(stack_submodels),
    grid = tibble(penalty = 10 ^ (-6:-1)),
    metrics = metric
  )

autoplot(stack_model_res)

stack_submodelsl_fit <-
  stack_model_spec %>%
  finalize_model(tibble(penalty = 10^-3)) %>%
  fit(Sale_Price ~ ., data = stack_submodels)

coef(stack_model_fit$fit, s = 10^-3) %>%
  as.matrix() %>%
  as.data.frame() %>%
  setNames("coefficient") %>%
  rownames_to_column("term") %>%
  as_tibble() %>%
  filter(coefficient > 0) %>%
  arrange(desc(coefficient))
