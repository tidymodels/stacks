#' Add members to a resample stack
#'
#' Add the workflow and resampling objects for potential ensemble members 
#' to a resample stack.
#'
#' @param stack A `stack` object.
#' @param members A model definition: either a `tune_results` 
#' or `resample_results` object outputted from
#' [tune::tune_grid()], [tune::tune_bayes()], or [tune::fit_resamples()]
#' @param name The label for the model definition---defaults to the name
#' of the model definition object.
#' @param workflow The workflow used to define the model definition.
#' @inheritParams stacks
#' 
#' @return A `stack` object--see [stacks()] for more details! 
#' 
#' @template note_example_data
#' 
#' @examples 
#' 
#' # initialize a stack and add some members
#' st <- 
#'   stacks() %>%
#'   add_members(lin_reg_res_, lin_reg_wf_) %>%
#'   add_members(svm_res_, svm_wf_) %>%
#'   add_members(spline_res_, spline_wf_)
#'   
#' @export
add_members <- function(stack, members, workflow,
                        name = deparse(substitute(members)), ...) {
  check_chr(name)
  
  stack <- 
    stack %>%
    set_rs_hash(members, name) %>%
    set_outcome(members) %>%
    set_training_data(members, name) %>%
    set_model_defs_members(members, workflow, name) %>%
    set_data_members(members, name)
  
  stack_constr(stack)
}
