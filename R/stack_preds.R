#' Collate predictions from a model stack
#' 
#' Given a model stack, this function collects the predictions
#' from each of the members' sub-models and collates them in a tibble.
#' This is a helper function and will generally not be used (explicitly) 
#' in a model stacking pipeline, but is exported for user convenience
#' for ad-hoc model stacking.
#' 
#' @inheritParams stack_eval
#' 
#' @return A tibble with `nrow(data)` rows and `1 + <number of sub-models>` 
#'   columns, where each column (besides the first, which contains the true 
#'   response) gives the prediction for each given sub-model.
#'
#' @template note_example_data  
#'  
#' @examples  
#' # initialize a model stack and add some members
#' st <- stack_init() %>%
#'   stack_add(svm_res_) %>%
#'   stack_add(spline_res_)
#'   
#' # collate model predictions
#' st %>%
#'   stack_preds(mtcars)
#'   
#' @export
stack_preds <- function(stack, data, ...) {
  outcome_name <- get_outcome(stack)
  
  outcome <- 
    data %>%
    dplyr::pull(outcome_name)
  
  assign(get_outcome(stack), outcome)
  
  res <- 
    purrr::map(
      stack$members,
      get_all_preds
    ) %>%
    dplyr::bind_cols() %>%
    dplyr::bind_cols(!!outcome_name := outcome, .) 
  
  tibble::as_tibble(res)
}



