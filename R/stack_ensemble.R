#' Fit a stack ensemble
#' 
#' @inheritParams stack_resamples
#' @param coefficient_stack A coefficient stack outputted by [stack_coefficients()]
# @param n The total number of sub-models to incorporate in the stack.
# @param replace Logical—whether to sample the sub-models to incorporate
# in the stack with replacement.
# @param n_initial The number of best sub-models to initialize the stack with
# before initiating subset selection.
# @param bag_p Numeric in (0, 1]—the proportion of models in the bag at
# each iteration.
#' @return A `ensemble_stack` object---this fitted model contains the necessary
#' components to predict on new data.
#' 
#' @template note_example_data
#' 
#' @examples 
#' 
#' # initialize a resample stack and add some members
#' st_res <- 
#'   stacks() %>%
#'   stack_resamples(lin_reg_res_) %>%
#'   stack_resamples(svm_res_) %>%
#'   stack_resamples(spline_res_)
#'   
#' # evaluate the resample stack
#' st_coefs <-
#'   st_res %>% 
#'   stack_data() %>%
#'   stack_coefficients()
#'   
#' stack_ensemble(st_res, st_coefs)
#' 
#' @export
stack_ensemble <- function(resample_stack, coefficient_stack) {
  
}