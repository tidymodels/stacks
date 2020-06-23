#' Collate predictions from a model stack
#' 
#' Given a model stack, this function collects the predictions
#' from each of the members' sub-models and collates them in a tibble.
#' 
#' @param stack A model `stack` object.
#' @param data The training data used to generate the resampling object.
#' @inheritParams model_stack
#' 
#' @return A tibble with `nrow(data)` rows and `1 + <number of sub-models>` 
#'   columns, where each column (besides the first, which contains the true 
#'   response) gives the prediction for each given sub-model.
#'
#' @template note_example_data  
#'  
#' @examples  
#' # initialize a model stack and add some members
#' st <- model_stack() %>%
#'   members_add(svm_res_) %>%
#'   members_add(spline_res_)
#'   
#' # collate model predictions
#' st %>%
#'   stack_data(mtcars)
#'   
#' @export
stack_data <- function(stack, data, ...) {
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
  
  structure(
    list(
      stacked_data = tibble::as_tibble(res)
    ),
    class = c("stacked_data", "list")
  )
}



