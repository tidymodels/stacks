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
  
  # formerly get_best_pred and get_all_pred in the draft spec
  
  # the documentation for metric and n are borrowed from show_best...
  # not sure if show_best or collect_predictions/metrics combined with some
  # dplyr will be a better route for filtering
  
  resamples <- list(...)
  
  outcome_colnames <- purrr::map_chr(resamples, tune::outcome_names)
  
  if (length(unique(outcome_colnames)) != 1) {
    stop("different outcome names depending on model")
  } 
  
  # grab the metrics for each model, subset the best n, and then
  # grab the predictions from those n
  
  # parse the metric and n arguments so that they can 
  # be recycled if needed in pmap
  metric_ <- if (length(metric) > 1) {
    metric
  } else {
    rep(metric, length(resamples))
  }
  
  n_ <- if (length(n) > 1) {n} else {rep(n, length(resamples))}
  
  best_models <- pmap(
    list(x = resamples,
         metric = metric_,
         n = n_),
    tune::show_best
  )
  
  # We want to subset the resamples from tune_* according to which
  # which were the best ^^^ and then collect predictions from them
  #pred <- map(best_models, tune::collect_predictions)
  
}



