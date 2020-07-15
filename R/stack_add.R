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
#' \donttest{
#' # put together a data stack using
#' # tuning results for regression models
#' reg_st <- 
#'   stacks() %>%
#'   stack_add(reg_res_lr) %>%
#'   stack_add(reg_res_svm) %>%
#'   stack_add(reg_res_sp)
#'   
#' # do the same with multinomial classification models
#' class_st <-
#'   stacks() %>%
#'   stack_add(class_res_nn) %>%
#'   stack_add(class_res_rf)
#'   
#' # ...or binomial classification models
#' log_st <-
#'   stacks() %>%
#'   stack_add(log_res_nn) %>%
#'   stack_add(log_res_rf)
#'   
#' # use custom names for each model:
#' log_st2 <-
#'   stacks() %>%
#'   stack_add(log_res_nn, name = "neural_network") %>%
#'   stack_add(log_res_rf, name = "random_forest")
#' }
#' 
#' @family core verbs
#' @export
stack_add <- function(data_stack, candidates,
                           name = deparse(substitute(candidates)), ...) {
  check_chr(name)
  
  stack <- 
    data_stack %>%
    .set_rs_hash(candidates, name) %>%
    .set_outcome(candidates) %>%
    .set_mode_(candidates, name) %>%
    .set_training_data(candidates, name) %>%
    .set_model_defs_candidates(candidates, name) %>%
    .set_data_candidates(candidates, name)
  
  if (data_stack_constr(stack)) {stack}
}
