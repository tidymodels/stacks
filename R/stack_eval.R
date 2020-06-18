#' Evaluate model stack coefficients
#'
#' Completes a stacked model specification by estimating the 
#' stacking/loading coefficients for the model stack. Model stack
#' evaluation assumes that the model members will no longer be
#' altered—to adjust member models, remove the model evaluation
#' with \code{stack_uneval()}.
#' 
#' @param stack A model `stack` object.
#' @param data The training data used to generate the resampling object.
#' @inheritParams stack_init
#' 
#' @return An evaluated model `stack` object.
#' 
#' @template note_example_data
#' 
#' @examples 
#' # initialize a model stack and add some members
#' st <- stack_init() %>%
#'   stack_add(svm_res_) %>%
#'   stack_add(spline_res_)
#' 
#' # evaluate the stack
#' st %>%
#'   stack_eval(mtcars)
#'   
#' # the following code will result in warnings about
#' # immutability of evaluated model stacks
#' \dontrun{
#' 
#' st %>%
#'   stack_eval(mtcars) %>%
#'   stack_rm(spline_res_)
#' }
#' 
#' # to alter an evaluated model stack, unevaluate the
#' # model before adding or removing models
#' st_eval <- st %>%
#'   stack_eval(mtcars)
#'   
#' st_eval %>%
#'   stack_uneval() %>%
#'   stack_rm(spline_res_)
#' 
#' @export
stack_eval <- function(stack, data, ...) {
  preds <- stack_preds(stack, data)
  
  preds_formula <- 
    paste0(colnames(preds)[1], " ~ .") %>%
    as.formula()
  
  stack_coefs <- 
    parsnip::linear_reg(penalty = tune::tune(), mixture = 1) %>%
    parsnip::set_engine("glmnet", lower.limits = 0) %>%
    tune::tune_grid(
      preds_formula,
      resamples = rsample::bootstraps(preds),
      grid = tibble::tibble(penalty = 10 ^ (-6:-1)),
      metrics = yardstick::metric_set(yardstick::rmse)
    )
  
  stack$coefficients <- stack_coefs
  
  stack_constr(stack)
}

#' @rdname stack_eval
#' @export
stack_uneval <- function(stack, ...) {
  stack["coefficients"] <- list(NULL)
  
  stack_constr(stack)
}
