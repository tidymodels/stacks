#' Evaluate a data stack
#'
#' Evaluates a stack by performing regularization on the out-of-sample
#' predictions to determine coefficients for the combining of predictions
#' from ensemble members.
#' 
#' @param data_stack A `data_stack` object
#' @inheritParams add_members
#' 
#' @return A `model_stack` object—while `model_stacks` largely contain the
#' same elements as `data_stack`s, the primary data objects shift from the
#' assessment set predictions to the member models.
#' 
#' @template note_example_data
#' 
#' @examples 
#' 
#' # put together a data stack
#' st <- 
#'   stacks() %>%
#'   add_members(lin_reg_res_, lin_reg_wf_) %>%
#'   add_members(svm_res_, svm_wf_) %>%
#'   add_members(spline_res_, spline_wf_)
#'   
#' # evaluate the data stack
#' st %>%
#'   linear_stack()
#' 
#' @export
linear_stack <- function(data_stack, ...) {
  preds_formula <- 
    paste0(colnames(data_stack)[1], " ~ .") %>%
    as.formula()
  
  model_spec <- 
    parsnip::linear_reg(penalty = tune::tune(), mixture = 1) %>%
    parsnip::set_engine("glmnet", lower.limits = 0)
  
  candidates <- 
    model_spec %>%
    tune::tune_grid(
      preds_formula,
      resamples = rsample::bootstraps(tibble::as_tibble(data_stack)),
      grid = tibble::tibble(penalty = 10 ^ (-6:-1)),
      metrics = yardstick::metric_set(yardstick::rmse),
      control = tune::control_grid(save_pred = TRUE)
    )
  
  coefs <-
    model_spec %>%
    tune::finalize_model(tune::select_best(candidates)) %>%
    generics::fit(formula = preds_formula, data = data_stack)
  
  model_stack <- 
    structure(
      list(model_defs = attr(data_stack, "model_defs"),
           coefs = coefs,
           cols_map = attr(data_stack, "cols_map"),
           model_metrics = attr(data_stack, "model_metrics"),
           train = attr(data_stack, "train"),
           outcome = attr(data_stack, "outcome")),
      class = c("linear_stack", "model_stack", "list")
    )
  
  model_stack_constr(model_stack)
}
