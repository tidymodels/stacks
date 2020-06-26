#' Add members to a resample stack
#'
#' Add the workflow and resampling objects for potential ensemble members 
#' to a resample stack.
#'
#' @param stack A `resample_stack` object.
#' @param members A model definition: either a `tune_results` 
#' or `resample_results` object outputted from
#' [tune::tune_grid()], [tune::tune_bayes()], or [tune::fit_resamples()]
#' @param name The label for the model definition---defaults to the name
#' of the model definition object.
#' @inheritParams pancakes
#' 
#' @return A `resample_stack` object--see [pancakes()] for more details! 
#' 
#' @template note_example_data
#' 
#' @examples 
#' 
#' # initialize a model stack
#' st <- pancakes()
#' 
#' # add some members to the stack
#' st <- st %>%
#'   stack_resample(lin_reg_res_) %>%
#'   stack_resamples(svm_res_) %>%
#'   stack_resamples(spline_res_)
#'   
#' @export
stack_resamples <- function(stack, members, 
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
