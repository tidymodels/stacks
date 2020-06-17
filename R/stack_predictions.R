#' Collate predictions from a model stack
#' 
#' Given a model stack, this function collects the predictions
#' from each of the members' sub-models and collates them in a tibble.
#' This is a helper function and will generally not be used (explicitly) 
#' in a model stacking pipeline, but is exported for user convenience
#' for ad-hoc model stacking.
#' 
#' @inheritParams stack_add
#' 
#' @return A tibble with `nrow(data)` rows and `1 + <number of sub-models>` 
#'   columns, where each column (besides the first, which contains the true 
#'   response) gives the prediction for each given sub-model.
#' @export
stack_predictions <- function(stack, ...) {
  
  res <- 
    purrr::map(
      stack$members,
      get_all_preds
    ) %>%
    dplyr::bind_cols() %>%
    dplyr::bind_cols(.row = 1:nrow(.), .)
  
}



