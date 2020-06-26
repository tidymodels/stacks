#' Add members to a resample stack
#'
#' Add the workflow and resampling objects for potential ensemble members 
#' to a resample stack.
#'
#' @param resample_stack A `resample_stack` object.
#' @param members A model definition: either a `tune_results` 
#' or `resample_results` object outputted from
#' [tune::tune_grid()], [tune::tune_bayes()], or [tune::fit_resamples()]
#' @param name The label for the model definition---defaults to the name
#' of the model definition object.
#' @inheritParams stacks
#' 
#' @return A `resample_stack` object--see [stacks()] for more details! 
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
#' @export
stack_resamples <- function(resample_stack, members, 
                            name = deparse(substitute(members)), ...) {
  check_chr(name)
  
  stack <- 
    stack %>%
    set_rs_hash(members, name) %>%
    set_outcome(members) %>%
    set_model_defs(members, name)
  
  stack <- collate_member(stack, members, name)
  
  stack_constr(stack)
}
