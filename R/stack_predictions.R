#' Collate predictions from resampling results
#' 
#' @param data Training data (as a data.frame or tibble)
#' @param ... Named resampling results (set of sub-models) outputted from 
#'   `tune_grid()`, where the name is an arbitrary and unique label for the 
#'   given set of sub-models. If `n > 1`, an integer will be appended to each 
#'   element sub-model in the set.
#' @param metric A character value giving the metric that will be used to sort 
#'   the models. (See
#'   \url{https://tidymodels.github.io/yardstick/articles/metric-types.html} for
#'   more details).
#' @param n An integer for the number of top sub-models to return. Either of
#'   length 1 if the same number of sub-models should be included from each
#'   set or of length equal to the number of sub-model set to specify how many
#'   sub-models to include from each sub-model set.
#' 
#' @return A tibble with `nrow(data)` rows and `1 + n * length(...)` columns,
#'   where each column (besides the first, which contains the true response) 
#'   gives the prediction for each selected sub-model.
#' 
#' @examples 
#' # just one sub-model from one sub-model set
#' stack_predictions(ames_train, glmnet = glmnet_res)
#'
#' # to include results from 5 sub-models from the glmnet sub-model set and 1 
#' # from mars: 
#' stack_predictions(
#'   ames_train, 
#'   glmnet = glmnet_res,
#'   mars = mars_res,
#'   n = c(5, 1)
#' )
stack_predictions <- function(data, ..., metric = "rmse", n = 1) {
  
}



