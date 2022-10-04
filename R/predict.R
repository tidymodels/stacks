#' Predicting with a model stack
#'
#' Apply a model stack to create different types of predictions.
#'
#' @param object A model stack with fitted members outputted from [fit_members()].
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
#' @examplesIf rlang::is_installed("ranger") & rlang::is_installed("kernlab")
#' \donttest{
#' 
#' # see the "Example Data" section above for
#' # clarification on the data and tuning results
#' # objects used in these examples!
#' 
#' data(tree_frogs_reg_test)
#' data(tree_frogs_class_test)
#' 
#' # build and fit a regression model stack
#' reg_st <-
#'   stacks() %>%
#'   add_candidates(reg_res_lr) %>%
#'   add_candidates(reg_res_sp) %>%
#'   blend_predictions() %>%
#'   fit_members()
#'
#' reg_st
#' 
#' # predict on the tree frogs testing data
#' predict(reg_st, tree_frogs_reg_test)
#' 
#' # include the predictions from the members
#' predict(reg_st, tree_frogs_reg_test, members = TRUE)
#' 
#' # build and fit a classification model stack
#' class_st <-
#'   stacks() %>%
#'   add_candidates(class_res_nn) %>%
#'   add_candidates(class_res_rf) %>%
#'   blend_predictions() %>%
#'   fit_members()
#'  
#' class_st
#' 
#' # predict reflex, first as a class, then as
#' # class probabilities
#' predict(class_st, tree_frogs_class_test)
#' predict(class_st, tree_frogs_class_test, type = "prob")
#' 
#' # returning the member predictions as well
#' predict(
#'   class_st, 
#'   tree_frogs_class_test, 
#'   type = "prob", 
#'   members = TRUE
#' )
#' 
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
  
  mode_suffix <- 
    switch(
      mode,
      classification = "classification",
      `censored regression` = ,
      regression = "regression"
    )
  
  member_preds <- 
    rlang::call2(
      paste0("predict_members_", mode_suffix),
      model_stack = object,
      coefs = coefs,
      new_data = new_data,
      opts = opts,
      type = member_type
    ) %>%
    rlang::eval_tidy() %>%
    setNames(., make.names(names(.)))
  
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

#' Predicting with a model stack
#' 
#' @description 
#' The data stack must be evaluated with [blend_predictions()] and its member 
#' models fitted with [fit_members()] to predict on new data. 
#' 
#' @param object A data stack.
#' @inheritParams stacks
#' 
#' @importFrom stats predict
#' @method predict data_stack
#' @export predict.data_stack
#' @export
predict.data_stack <- function(object, ...) {
  cli_abort(
    "To predict with a stacked ensemble, the supplied data stack must be 
     evaluated with `blend_predictions()` and its member models fitted with 
     `fit_members()` to predict on new data.",
    call = caller_env(0)
  )
}

check_pred_type <- function(object, type) {
  if (is.null(type)) {
    if (object[["mode"]] %in% c("regression", "censored regression")) {
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
    cli_abort(
      "The supplied model stack hasn't been fitted yet. 
       Please fit the necessary members with fit_members() to predict on new data.",
      call = caller_env()
    )
  }
}
