#' Evaluate a data stack
#'
#' Evaluates a stack by performing regularization on the out-of-sample
#' predictions to determine coefficients for the combining of predictions
#' from ensemble members.
#' 
#' @param data_stack A `data_stack` object
#' @inheritParams stack_add
#' 
#' @return A `model_stack` object—while `model_stacks` largely contain the
#' same elements as `data_stack`s, the primary data objects shift from the
#' assessment set predictions to the member models.
#' 
#' @template note_example_data
#' 
#' @examples 
#' \donttest{
#' # put together a data stack
#' st <- 
#'   stacks() %>%
#'   stack_add(reg_res_lr) %>%
#'   stack_add(reg_res_svm) %>%
#'   stack_add(reg_res_sp)
#'   
#' # evaluate the data stack
#' st %>%
#'   stack_blend()
#' }
#' @export
stack_blend <- function(data_stack, ...) {
  preds_formula <- 
    paste0(colnames(data_stack)[1], " ~ .") %>%
    as.formula()
  
  if (attr(data_stack, "mode") == "regression") {
    model_spec <- 
      parsnip::linear_reg(penalty = tune::tune(), mixture = 1) %>%
      parsnip::set_engine("glmnet", lower.limits = 0)
    
    metric <- yardstick::metric_set(yardstick::rmse)
  } else {
    if (length(unique(dplyr::pull(tibble::as_tibble(data_stack)[,1]))) == 2) {
      model_spec <-
        parsnip::logistic_reg(penalty = tune::tune(), mixture = 1) %>% 
        parsnip::set_engine("glmnet", lower.limits = 0) %>% 
        parsnip::set_mode("classification")
    } else {
      model_spec <-
        parsnip::multinom_reg(penalty = tune::tune(), mixture = 1) %>% 
        parsnip::set_engine("glmnet", lower.limits = 0) %>% 
        parsnip::set_mode("classification")
    }
    metric <- yardstick::metric_set(yardstick::roc_auc)
  }
  
  candidates <- 
    model_spec %>%
    tune::tune_grid(
      preds_formula,
      resamples = rsample::bootstraps(tibble::as_tibble(data_stack)),
      grid = tibble::tibble(penalty = 10 ^ (-6:-1)),
      metrics = metric,
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
           mode = attr(data_stack, "mode"),
           outcome = attr(data_stack, "outcome"),
           data_stack = tibble::as_tibble(data_stack)),
      class = c("linear_stack", "model_stack", "list")
    )
  
  if (model_stack_constr(model_stack)) {model_stack}
}
