#' Model predictions
#'
#' Apply a model stack to create different types of predictions.
#'
#' @param object A model stack with fitted members.
#' @param new_data A rectangular data object, such as a data frame.
#' @param type Format of returned predicted values—one of "numeric", "class",
#'   "prob", or "members". When NULL, `predict()` will
#'   choose an appropriate value based on the model's mode.
#' @param opts A list of optional arguments to the underlying predict 
#'   function passed on to [parsnip::predict.model_fit] for each member.
#' @inheritParams stacks
#'
#' @template note_example_data
#'
#' @examples 
#' \donttest{
#' # build and fit a regression model stack
#' reg_st <-
#'   stacks() %>%
#'   stack_add(reg_res_lr) %>%
#'   stack_add(reg_res_sp) %>%
#'   stack_blend() %>%
#'   stack_fit()
#' 
#' # predict on the penguins data
#' predict(reg_st, penguins)
#' 
#' # build and fit a classification model stack
#' class_st <-
#'   stacks() %>%
#'   stack_add(class_res_nn) %>%
#'   stack_add(class_res_rf) %>%
#'   stack_blend() %>%
#'   stack_fit()
#' 
#' # predict species, first as a class, then as
#' # class probabilities
#' predict(class_st, penguins)
#' predict(class_st, penguins, type = "prob")
#' }
#'
#' @importFrom stats predict
#' @method predict model_stack
#' @export predict.model_stack
#' @export
predict.model_stack <- function(object, new_data, type = NULL, opts = list(), ...) {
  type <- check_pred_type(object, type)
  mode <- object[["mode"]]
    
  coefs <- 
    .get_glmn_coefs(object[["coefs"]][["fit"]]) %>%
    dplyr::select(terms, estimate)
  
  do.call(
    paste0("predict_", object[["mode"]], "_", type),
    list(
      model_stack = object,
      coefs = coefs,
      new_data = new_data,
      opts = opts
    )
  )
}

check_pred_type <- function(object, type) {
  if (is.null(type)) {
    if (object[["mode"]] == "regression") {
      type <- "numeric"
    } else {
      type <- "class"
    }
  }
  
  type
}

predict_regression_numeric <- function(model_stack, coefs, new_data, opts, ...) {
  # predict using each member model
  prediction_members <- 
    predict_regression_members(model_stack, coefs, new_data, opts, ...)
  
  term_coefs <-
    coefs %>%
    dplyr::filter(estimate !=0 & terms != "(Intercept)") %>%
    tidyr::pivot_wider(values_from = estimate, names_from = terms)
  
  # multiply the predictions by the appropriate coefficients
  predictions <- 
    mapply("*", prediction_members, term_coefs) %>%
    rowSums() %>%
    `+`(
      coefs %>%
        dplyr::filter(terms == "(Intercept)") %>%
        dplyr::pull()
    )
  
  predictions
}

predict_regression_members <- function(model_stack, coefs, new_data, opts, ...) {
  predictions <- 
    purrr::map(
      model_stack[["member_fits"]],
      predict,
      new_data = new_data,
      type = "numeric",
      opts = opts
    ) %>%
    purrr::map(dplyr::pull) %>%
    tibble::as_tibble()
  
  predictions
}

predict_classification_prob <- function(model_stack, coefs, new_data, opts, ...) {
  cols_map_tibble <-
    tibble::enframe(model_stack[["cols_map"]]) %>% 
    tidyr::unnest(cols = value) %>%
    dplyr::mutate(sanitize_classification_names(
      model_stack, 
      value
    )) %>%
    dplyr::select(
      model = name,
      member = new,
      term = value
    )
  
  term_coefs <-
    coefs %>%
    dplyr::filter(estimate !=0 & terms != "(Intercept)")
  
  predictions <- 
    purrr::map(
      model_stack[["member_fits"]],
      predict,
      new_data = new_data,
      type = "prob",
      opts = opts
    ) %>%
    purrr::map(tibble::rowid_to_column) %>%
    tibble::enframe() %>%
    tidyr::unnest(cols = value) %>%
    tidyr::pivot_longer(
      cols = contains(".pred"), 
      names_to = "pred_class",
      values_to  = "estimate"
    ) %>%
    tidyr::unite("term", c(pred_class, name), sep = "_", remove = FALSE) %>%
    dplyr::inner_join(term_coefs, by = c("term" = "terms")) %>%
    dplyr::mutate(weighted_est = estimate.x * estimate.y) %>%
    dplyr::select(rowid, name, pred_class, weighted_est) %>%
    dplyr::group_by(rowid, pred_class) %>%
    dplyr::summarize(pred_class_sum = sum(weighted_est), .groups = "drop") %>%
    dplyr::group_by(rowid) %>%
    dplyr::mutate(pred_class_sum_norm = pred_class_sum / sum(pred_class_sum)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-pred_class_sum) %>%
    tidyr::pivot_wider(
      id_cols = c(rowid), 
      names_from = pred_class, 
      values_from = pred_class_sum_norm
    ) %>%
    dplyr::arrange(rowid) %>%
    dplyr::select(-rowid)
  
  predictions
}

predict_classification_class <- function(model_stack, coefs, new_data, opts, ...) {
  prediction_probs <- 
    predict_classification_prob(
      model_stack,
      coefs,
      new_data,
      opts,
      ...
    )
  
  predictions <- 
    prediction_probs %>%
    tibble::rowid_to_column() %>%
    tidyr::pivot_longer(cols = c(dplyr::everything(), -rowid)) %>%
    dplyr::group_by(rowid) %>%
    dplyr::filter(value == max(value)) %>%
    dplyr::arrange(rowid) %>%
    dplyr::pull(name) %>%
    stringi::stri_replace_all_fixed(".pred_", "")
  
  predictions
}

predict_classification_members <- function(model_stack, coefs, new_data, opts, ...) {
  predictions <- 
    purrr::map(
      model_stack[["member_fits"]],
      predict,
      new_data = new_data,
      type = "class",
      opts = opts
    ) %>%
    purrr::map(dplyr::pull) %>%
    tibble::as_tibble()
  
  predictions
}

#' @importFrom generics augment
#' @export
generics::augment

#' A barebones augment method to help with testing.
#' 
#' @param x A `model_stack` object
#' @param data A `data.frame`-like object to collect predictions on.
#' @inheritParams stacks
#' 
#' @importFrom generics augment
#' @method augment model_stack
#' @export augment.model_stack
#' @export
augment.model_stack <- function(x, data = x[["train"]], ...) {
  data$.fitted <- predict(x, data)
  
  data
}
