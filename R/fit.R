#' @importFrom generics fit
#' @export
generics::fit

#' Fit an evaluated model stack.
#' 
#' @param object Stacked data.
# @param n The total number of sub-models to incorporate in the stack.
# @param replace Logical—whether to sample the sub-models to incorporate
# in the stack with replacement.
# @param n_initial The number of best sub-models to initialize the stack with
# before initiating subset selection.
# @param bag_p Numeric in (0, 1]—the proportion of models in the bag at
# each iteration.
#' @inheritParams pancakes
#' 
#' @rdname fit
#' @export
fit.stack <- function(object, train_dat, ...) {
  preds_formula <- 
    paste0(colnames(object)[1], " ~ .") %>%
    as.formula()
  
  model_spec <- 
    parsnip::linear_reg(penalty = tune::tune(), mixture = 1) %>%
    parsnip::set_engine("glmnet", lower.limits = 0)
  
  stack_coefs <- 
    model_spec %>%
    tune::tune_grid(
      preds_formula,
      resamples = rsample::bootstraps(tibble::as_tibble(object)),
      grid = tibble::tibble(penalty = 10 ^ (-6:-1)),
      metrics = yardstick::metric_set(yardstick::rmse),
      control = tune::control_grid(save_pred = TRUE)
    )
  
  fit <-
    model_spec %>%
    tune::finalize_model(tune::select_best(stack_coefs)) %>%
    generics::fit(formula = preds_formula, data = penguins_train)
    
}
