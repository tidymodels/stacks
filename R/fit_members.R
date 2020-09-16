#' Fit model stack members
#' 
# @param n The total number of sub-models to incorporate in the stack.
# @param replace Logical—whether to sample the sub-models to incorporate
# in the stack with replacement.
# @param n_initial The number of best sub-models to initialize the stack with
# before initiating subset selection.
# @param bag_p Numeric in (0, 1]—the proportion of models in the bag at
# each iteration.
#
#' @param model_stack A `model_stack` object outputted by `blend_predictions()` or
#'   `fit_members()`
#' @inheritParams stacks
#' @inheritParams blend_predictions
#' @return A `model_stack` object with a subclass inherited from the chosen
#' `*_stack` method---this fitted model contains the 
#' necessary components to predict on new data.
#' 
#' @template note_example_data
#' 
#' @examples 
#' \donttest{
#' # see the "Example Data" section above for
#' # clarification on the objects used in these examples!
#' 
#' # put together a data stack
#' reg_st <- 
#'   stacks() %>%
#'   add_candidates(reg_res_lr) %>%
#'   add_candidates(reg_res_svm) %>%
#'   add_candidates(reg_res_sp)
#'   
#' reg_st
#'
#' # evaluate the data stack and fit the member models
#' reg_st %>%
#'   blend_predictions() %>%
#'   fit_members()
#'   
#' reg_st
#'   
#' # do the same with multinomial classification models
#' class_st <-
#'   stacks() %>%
#'   add_candidates(class_res_nn) %>%
#'   add_candidates(class_res_rf) %>%
#'   blend_predictions() %>%
#'   fit_members()
#'   
#' class_st
#'   
#' # ...or binomial classification models
#' log_st <-
#'   stacks() %>%
#'   add_candidates(log_res_nn) %>%
#'   add_candidates(log_res_rf) %>%
#'   blend_predictions() %>%
#'   fit_members()
#'   
#' log_st
#' }
#' 
#' @family core verbs
#' @export
fit_members <- function(model_stack, verbose = FALSE, ...) {
  check_model_stack(model_stack)
  
  dat <- model_stack[["train"]]
  
  # pick out which submodels have nonzero coefs
  member_names <- 
    .get_glmn_coefs(
      model_stack[["coefs"]][["fit"]], 
      model_stack[["coefs"]][["spec"]][["args"]][["penalty"]]
    ) %>%
    dplyr::filter(estimate != 0 & terms != "(Intercept)") %>%
    dplyr::pull(terms)
  
  if (model_stack[["mode"]] == "classification") {
    member_dict <- 
      sanitize_classification_names(model_stack, member_names)
    
    member_names <- member_dict$new %>% unique()
  }
  
  # make model specs with the chosen parameters
  # for chosen sub-models
  metrics_dict <- 
    tibble::enframe(model_stack[["model_metrics"]]) %>%
    tidyr::unnest(cols = value) %>%
    dplyr::mutate(
      .config = gsub(
        pattern = c("Model|Recipe"),
        replacement = "",
        x = .config,
      ),
      .config = dplyr::case_when(
        !is.na(.config) ~ paste0(name, .config),
        TRUE ~ paste0(name, "1")
      )
    ) %>%
    dplyr::filter(.metric %in% c("rmse", "roc_auc"))
  
  if (model_stack[["mode"]] == "regression") {
    members_map <- 
      tibble::enframe(model_stack[["cols_map"]]) %>%
      tidyr::unnest(cols = value) %>%
      dplyr::full_join(metrics_dict, by = c("value" = ".config"))
  } else {
    members_map <- 
      tibble::enframe(model_stack[["cols_map"]]) %>%
      tidyr::unnest(cols = value) %>%
      dplyr::full_join(member_dict, by = c("value" = "old")) %>%
      dplyr::filter(!is.na(new)) %>%
      dplyr::select(name, value = new) %>%
      dplyr::filter(!duplicated(.$value)) %>%
      dplyr::full_join(metrics_dict, by = c("value" = ".config"))
  }
  
  # fit each of them
  member_fits <- 
    purrr::map(
      member_names,
      fit_member,
      wflows = model_stack[["model_defs"]],
      members_map = members_map,
      train_dat = dat
    )
  
  model_stack[["member_fits"]] <- 
    setNames(member_fits, member_names)
  
  if (model_stack_constr(model_stack)) {model_stack}
}


# fit one member of the ensemble
fit_member <- function(name, wflows, members_map, train_dat) {
  member_row <- 
    members_map %>%
    dplyr::filter(value == name)
  
  member_params <- 
    wflows[[member_row$name.x]] %>%
    dials::parameters() %>%
    dplyr::pull(id)
  
  needs_finalizing <- length(member_params) != 0
  
  if (needs_finalizing) {
    member_metrics <-
      members_map %>%
      dplyr::filter(value == name)
    
    member_wf <- 
      wflows[[member_metrics$name.x]]
    
    new_member <- 
      tune::finalize_workflow(member_wf, member_metrics[,member_params]) %>%
      generics::fit(data = train_dat)
  } else {
    member_model <-
      members_map %>%
      dplyr::filter(value == name) %>%
      dplyr::select(name.x) %>%
      dplyr::pull()
    
    new_member <-
      generics::fit(wflows[[member_model[1]]], data = train_dat)
  }
  
  new_member
}

# creates a map for column / entry names resulting
# from tuning in the classification setting
sanitize_classification_names <- function(model_stack, member_names) {
  outcome_levels <-
    model_stack[["train"]] %>%
    dplyr::select(!!.get_outcome(model_stack)) %>%
    dplyr::pull() %>%
    as.character() %>%
    unique()
  
  pred_strings <- paste0(".pred_", outcome_levels, "_")
  
  new_member_names <-
    gsub(
      pattern = paste0(pred_strings, collapse = "|"),
      x = member_names,
      replacement = ""
    )
  
  tibble::tibble(
    old = member_names,
    new = new_member_names
  )
}


check_model_stack <- function(model_stack) {
  if (inherits(model_stack, "model_stack")) {
    return(invisible(TRUE))
  } else if (inherits(model_stack, "data_stack")) {
    glue_stop(
      "The supplied `model_stack` argument is a data stack rather than ",
      "a model stack. Did you forget to first evaluate the data stack's ",
      "blending coefficients with `blend_predictions()`?"
    )
  } else {
    check_inherits(model_stack, "model_stack")
  }
}
