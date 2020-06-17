#' Evaluate model stack coefficients
#'
#' Completes a stacked model specification by estimating the 
#' stacking/loading coefficients for the model stack.
#' 
#' @param stack A model `stack` object.
#' @inheritParams stack_init
#' 
#' @return 
#' 
#' @examples 
#' 
#' #example man-roxygen/ex-note_example_data
#' 
#' # initialize a model stack and add some members
#' st <- stack_init() %>%
#'   stack_add(svm_res_) %>%
#'   stack_add(spline_res_)
#' 
#' # evaluate the stack
#' st %>%
#'   stack_eval()
#' 
#' @export
stack_eval <- function(stack, preprocessor, ...) {

  stacked_predictions <- stack_predictions(stack)
  
  parsnip::linear_reg(penalty = tune::tune(), mixture = 1) %>%
    parsnip::set_engine("glmnet", lower.limits = 0) %>%
    tune::tune_grid(
      preprocessor,
      resamples = bootstraps(stacked_predictions),
      grid = tibble(penalty = 10 ^ (-6:-1)),
      metrics = metric
    )
}
