#' Add model definitions to a data stack
#'
#' Collates assessment set predictions and appends workflows and additional 
#' attributes to a `data_stack`.
#'
#' @param data_stack A `data_stack` object.
#' @param candidates A model definition: either a `tune_results` 
#' or `resample_results` object outputted from
#' [tune::tune_grid()], [tune::tune_bayes()], or [tune::fit_resamples()].
#' These results must have been fitted with the `control` settings
#' `save_pred = TRUE, save_workflow = TRUE`—see the [control_stack_grid()],
#' [control_stack_bayes()], and [control_stack_resamples()]
#' documentation for helper functions.
#' @param name The label for the model definition---defaults to the name
#' of the `candidates` object.
#' @inheritParams stacks
#' 
#' @return A `data_stack` object--see [stacks()] for more details! 
#' 
#' @template note_example_data
#' 
#' @examples 
#' 
#' # initialize a stack and add some members
#' st <- 
#'   stacks() %>%
#'   add_candidates(reg_res_lr) %>%
#'   add_candidates(reg_res_svm) %>%
#'   add_candidates(reg_res_sp)
#'   
#' # do the same with classification models
#' st <- 
#'   stacks() %>%
#'   add_candidates(class_res_nn) %>%
#'   add_candidates(class_res_rf)  
#' @export
add_candidates <- function(data_stack, candidates,
                           name = deparse(substitute(candidates)), ...) {
  check_chr(name)
  
  stack <- 
    data_stack %>%
    set_rs_hash(candidates, name) %>%
    set_outcome(candidates) %>%
    set_mode_(candidates, name) %>%
    set_training_data(candidates, name) %>%
    set_model_defs_members(candidates, name) %>%
    set_data_members(candidates, name)
  
  data_stack_constr(stack)
}
