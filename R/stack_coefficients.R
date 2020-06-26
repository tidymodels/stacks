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
  
}