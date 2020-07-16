#' Model predictions
#'
#' Apply a model stack to create different types of predictions.
#'
#' @param object A model stack with fitted members.
#' @param new_data A rectangular data object, such as a data frame.
#' @param type Format of returned predicted values—one of "numeric", "class",
#'   or "prob". When NULL, `predict()` will
#'   choose an appropriate value based on the model's mode.
#' @param members Logical. Whether or not to additionally return the predictions
#'   for each of the ensemble members.
#' @param opts A list of optional arguments to the underlying predict 
#'   function passed on to [parsnip::predict.model_fit] for each member.
#' @inheritParams stacks
#'
#' @template note_example_data
#'
#' @examples 
#' \donttest{
#' data(penguins_test)
#' 
#' # build and fit a regression model stack
#' reg_st <-
#'   stacks() %>%
#'   stack_add(reg_res_lr) %>%
#'   stack_add(reg_res_sp) %>%
#'   stack_blend() %>%
#'   stack_fit()
#' 
#' # predict on the penguins testing data
#' predict(reg_st, penguins_test)
#' 
#' # include the predictions from the members
#' predict(reg_st, penguins_test, members = TRUE)
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
#' predict(class_st, penguins_test)
#' predict(class_st, penguins_test, type = "prob")
#' }
#'
#' @importFrom stats predict
#' @method predict model_stack
#' @export predict.model_stack
#' @export
predict.model_stack <- function(object, new_data, type = NULL, members = FALSE, 
                                opts = list(), ...) {
  type <- check_pred_type(object, type)
  
  coefs <- 
    .get_glmn_coefs(object[["coefs"]][["fit"]]) %>%
    dplyr::select(terms, estimate)
  
  member_preds <- 
    rlang::call2(
      paste0("predict_members_", object[["mode"]]),
      model_stack = object,
      coefs = coefs,
      new_data = new_data,
      opts = opts
    ) %>%
    rlang::eval_tidy()
  
  # will now need to combine member predictions
  
  invisible(TRUE)
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

predict_members_regression <- function(model_stack, coefs, new_data, opts) {
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

predict_members_classification <- function(model_stack, coefs, new_data, opts) {
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
    tidyr::pivot_wider(id_cols = rowid,
                       names_from = name,
                       values_from = 3:ncol(.)) %>%
    dplyr::select(-rowid)
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
