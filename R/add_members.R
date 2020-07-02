#' Add model definitions to a data stack
#'
#' Collates assessment set predictions and appends workflows and additional 
#' attributes to a `data_stack`.
#'
#' @param data_stack A `data_stack` object.
#' @param members A model definition: either a `tune_results` 
#' or `resample_results` object outputted from
#' [tune::tune_grid()], [tune::tune_bayes()], or [tune::fit_resamples()]
#' @param workflow The workflow used to define the model definition.
#' @param name The label for the model definition---defaults to the name
#' of the `members` object.
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
#'   add_members(reg_res_lr, reg_wf_lr) %>%
#'   add_members(reg_res_svm, reg_wf_svm) %>%
#'   add_members(reg_res_sp, reg_wf_sp)
#'   
#' # do the same with classification models
#' st <- 
#'   stacks() %>%
#'   add_members(class_res_nn, class_wf_nn) %>%
#'   add_members(class_res_rf, class_wf_rf)  
#' @export
add_members <- function(data_stack, members, workflow,
                        name = deparse(substitute(members)), ...) {
  check_chr(name)
  
  stack <- 
    data_stack %>%
    set_rs_hash(members, name) %>%
    set_outcome(members) %>%
    set_mode_(workflow, name) %>%
    set_training_data(members, name) %>%
    set_model_defs_members(members, workflow, name) %>%
    set_data_members(members, name)
  
  data_stack_constr(stack)
}
