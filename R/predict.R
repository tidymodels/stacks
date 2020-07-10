#' Model predictions
#'
#' Apply a model stack to create different types of predictions.
#'
#' @param object A model stack with fitted members.
#' @param new_data A rectangular data object, such as a data frame.
#' @param type Format of returned predicted values—passed on to 
#'   [parsnip::predict.model_fit] for each member.
#' @param opts A list of optional arguments to the underlying predict 
#'   function passed on to [parsnip::predict.model_fit] for each member.
#' @inheritParams stacks
#'
#' @importFrom stats predict
#' @method predict model_stack
#' @export predict.model_stack
#' @export
predict.model_stack <- function(object, new_data, type = NULL, opts = list(), ...) {
  # predict using each member model
  predictions <- 
    purrr::map(
      object[["member_fits"]],
      predict,
      new_data = new_data,
      type = type,
      opts = opts
    ) %>%
    purrr::map(dplyr::pull) %>%
    tibble::as_tibble()
  
  # extract the stacking coefficients
  coefs <- 
    .get_glmn_coefs(object[["coefs"]][["fit"]]) %>%
    dplyr::select(terms, estimate)
  
  term_coefs <-
    coefs %>%
    dplyr::filter(estimate !=0 & terms != "(Intercept)") %>%
    tidyr::pivot_wider(values_from = estimate, names_from = terms)
  
  # multiply the predictions by the appropriate coefficients
  res <- 
    mapply("*", predictions, term_coefs) %>%
    rowSums() %>%
    `+`(
      coefs %>%
        dplyr::filter(terms == "(Intercept)") %>%
        dplyr::pull()
    )
  
  res
}
