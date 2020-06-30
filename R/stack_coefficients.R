#' Evaluate a stack
#'
#' Evaluates a stack by performing regularization on the out-of-sample
#' predictions to determine coefficients for the combining of predictions
#' from ensemble members.
#' 
#' @param data_stack A `data_stack` object outputted from [stack_data()]
#' @inheritParams add_members
#' 
#' @return A `stack` object---a `model_fit` subclass, giving loading
#' coefficients for each ensemble member.
#' 
#' @template note_example_data
#' 
#' @examples 
#' 
#' # evaluate a stack
#' st <- 
#'   stacks() %>%
#'   add_members(lin_reg_res_) %>%
#'   add_members(svm_res_) %>%
#'   add_members(spline_res_)
#'   
#' # evaluate the stack
#' st %>%
#'   stack_coefficients()
#' 
#' @export
stack_coefficients <- function(stack, method = "glm", ...) {
  preds_formula <- 
    paste0(colnames(stack)[1], " ~ .") %>%
    as.formula()
  
  model_spec <- 
    parsnip::linear_reg(penalty = tune::tune(), mixture = 1) %>%
    parsnip::set_engine("glmnet", lower.limits = 0)
  
  candidates <- 
    model_spec %>%
    tune::tune_grid(
      preds_formula,
      resamples = rsample::bootstraps(tibble::as_tibble(stack)),
      grid = tibble::tibble(penalty = 10 ^ (-6:-1)),
      metrics = yardstick::metric_set(yardstick::rmse),
      control = tune::control_grid(save_pred = TRUE)
    )
  
  coefs <-
    model_spec %>%
    tune::finalize_model(tune::select_best(candidates)) %>%
    generics::fit(formula = preds_formula, data = stack)
  
  ensemble <- 
    structure(
      list(model_defs = attr(stack, "model_defs"),
           coefs = coefs,
           cols_map = attr(stack, "cols_map"),
           model_metrics = attr(stack, "model_metrics"),
           train = attr(stack, "train")),
      class = c("ensemble", "list")
    )
  
  ensemble_constr(ensemble)
}
