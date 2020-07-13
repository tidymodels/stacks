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
  
  res <- 
    prediction_probs %>%
    tibble::rowid_to_column() %>%
    tidyr::pivot_longer(cols = c(dplyr::everything(), -rowid)) %>%
    dplyr::group_by(rowid) %>%
    dplyr::filter(value == max(value)) %>%
    dplyr::arrange(rowid) %>%
    dplyr::pull(name) %>%
    stringi::stri_replace_all_fixed(".pred_", "")
}

#' @importFrom generics augment
#' @export
generics::augment

# A barebones augment method to help with testing.
#' @method augment model_stack
#' @export augment.model_stack
augment.model_stack <- function(x, data = x[["train"]], ...) {
  data$.fitted <- predict(x, data)
  
  data
}




