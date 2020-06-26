#' Evaluate a data stack
#'
#' Evaluates a data stack by performing regularization on the out-of-sample
#' predictions to determine coefficients for the combining of predictions
#' from ensemble members.
#' 
#' @param data_stack A `data_stack` object outputted from [stack_data()]
#' @inheritParams stack_resamples
#' 
#' @return A `data_stack` object---a `model_fit` subclass, giving loading
#' coefficients for each ensemble member.
#' 
#' @template note_example_data
#' 
#' @examples 
#' 
#' # evaluate a resample stack
#' st <- 
#'   stacks() %>%
#'   stack_resamples(lin_reg_res_) %>%
#'   stack_resamples(svm_res_) %>%
#'   stack_resamples(spline_res_) %>% 
#'   stack_data()
#'   
#' # evaluate the data stack
#' st %>%
#'   stack_coefficients()
#' 
#' @export
stack_coefficients <- function(stack_data, ...) {
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
    generics::fit(formula = preds_formula, data = train_dat)
}