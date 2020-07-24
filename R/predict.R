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
#' # see the "Example Data" section above for
#' # clarification on the data and tuning results
#' # objects used in these examples!
#' 
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
#' reg_st
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
#' class_st
#' 
#' # predict year, first as a class, then as
#' # class probabilities
#' predict(class_st, penguins_test)
#' predict(class_st, penguins_test, type = "prob")
#' 
#' # returning the member predictions instead
#' predict(
#'   class_st, 
#'   penguins_test, 
#'   type = "prob", 
#'   members = TRUE
#' )
#' }
#'
#' @importFrom stats predict
#' @method predict model_stack
#' @export predict.model_stack
#' @export
predict.model_stack <- function(object, new_data, type = NULL, members = FALSE, 
                                opts = list(), ...) {
  check_fitted(object)
  type <- check_pred_type(object, type)
  check_inherits(members, "logical")
  check_inherits(opts, "list")
  
  coefs <- 
    .get_glmn_coefs(object[["coefs"]][["fit"]]) %>%
    dplyr::select(terms, estimate)
  
  member_type <- 
    switch(type,
           class =, prob = "prob",
           numeric = "numeric")
  
  member_preds <- 
    rlang::call2(
      paste0("predict_members_", object[["mode"]]),
      model_stack = object,
      coefs = coefs,
      new_data = new_data,
      opts = opts,
      type = member_type
    ) %>%
    rlang::eval_tidy()
  
  res <- stack_predict(object$equations[[type]], member_preds)
  
  if (members) {
    if (type == "class") {
      member_preds <- 
        purrr::map_dfc(
          names(object[["member_fits"]]),
          parse_member_probs,
          member_preds,
          attr(new_data[[object[["outcome"]]]], "levels")
        )
    }
    res <- dplyr::bind_cols(res, member_preds)
  }
  
  res
}

#' Apply a stacked_ensemble to create different types of predictions.
#' 
#' @param object A data stack.
#' @inheritParams stacks
#' 
#' @importFrom stats predict
#' @method predict data_stack
#' @export predict.data_stack
#' @export
predict.data_stack <- function(object, ...) {
  glue_stop(
    "To predict with a stacked ensemble, the supplied data stack must be ",
    "evaluated with `stack_blend()` and its member models fitted with ",
    "`stack_fit()` to predict on new data."
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

predict_members_regression <- function(model_stack, coefs, new_data, opts, type) {
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

predict_members_classification <- function(model_stack, coefs, new_data, opts, type) {
  levels <- attr(new_data[[model_stack[["outcome"]]]], "levels")
  
  member_preds <- 
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
  
  member_preds
}

parse_member_probs <- function(member_name, member_probs, levels) {
  member_probs[, grepl(member_name, colnames(member_probs))] %>%
    multi_net_helper() %>%
    dplyr::transmute(
      !!paste0(".pred_class_", member_name) := factor(levels[idx], levels = levels)
    )
}

check_fitted <- function(model_stack) {
  if (is.null(model_stack[["member_fits"]])) {
    glue_stop(
      "The supplied model stack hasn't been fitted yet. ",
      "Please fit the necessary members with stack_fit() to predict on new data."
    )
  }
}