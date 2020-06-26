#' Evaluate a resample stack
#'
#' Evaluates a resample stack by collating out-of-sample predictions
#' from each potential ensemble member.
#'
#' @inheritParams stack_resamples
#' 
#' @return A `data_stack` object---a `tbl_df` subclass, where the first column
#' gives the true outcome in the training data, and the remaining columns give
#' the out-of-sample predictions from each potential ensemble member.
#' 
#' @template note_example_data
#' 
#' @examples 
#' 
#' # initialize a resample stack and add some members
#' st <- 
#'   stacks() %>%
#'   stack_resamples(lin_reg_res_) %>%
#'   stack_resamples(svm_res_) %>%
#'   stack_resamples(spline_res_)
#'   
#' # evaluate the resample stack
#' st %>% 
#'   stack_data()
#'   
#' @export
stack_data <- function(resample_stack, ...) {
  
}