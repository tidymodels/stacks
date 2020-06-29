#' Fit a stack ensemble
#' 
#' @inheritParams add_members
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
#' # initialize a stack
#' st_res <- 
#'   stacks() %>%
#'   add_members(lin_reg_res_, lin_reg_wf_) %>%
#'   add_members(svm_res_, svm_wf_) %>%
#'   add_members(spline_res_, spline_wf_) %>%
#'   stack_coefficients()
#'
#' # evaluate and build an ensemble
#' stack_ensemble(st_res, st_coefs)
#' 
#' @export
stack_ensemble <- function(stack, data) {
  # pick out which models have nonzero coefs
  coefs <- get_glmn_coefs(attr(stack, "coefs")[["fit"]])
  
  # fit each of them
  member_fits <- 
    purrr::map(
      attr(stack, "model_defs"),
      generics::fit,
      data = data
    )
  
  structure(
    list(
      coefs = coefs,
      member_fits = fits
    ),
    class = c("ensemble", "list")
  )
}
