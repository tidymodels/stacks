#' Fit model stack members
#' 
#' @inheritParams add_members
# @param n The total number of sub-models to incorporate in the stack.
# @param replace Logical—whether to sample the sub-models to incorporate
# in the stack with replacement.
# @param n_initial The number of best sub-models to initialize the stack with
# before initiating subset selection.
# @param bag_p Numeric in (0, 1]—the proportion of models in the bag at
# each iteration.
#' @return A `model_stack` object with a subclass inherited from the chosen
#' `*_stack` method---this fitted model contains the 
#' necessary components to predict on new data.
#' 
#' @template note_example_data
#' 
#' @examples 
#' 
#' # put together a data stack
#' st <- 
#'   stacks() %>%
#'   add_members(reg_res_lr, reg_wf_lr) %>%
#'   add_members(reg_res_svm, reg_wf_svm) %>%
#'   add_members(reg_res_sp, reg_wf_sp)
#'
#' # evaluate the data stack and fit the member models
#' st %>%
#'   linear_stack() %>%
#'   fit_members()
#' 
# ensemble <- stacks() %>% 
# add_members(reg_res_svm, reg_wf_svm) %>% 
# add_members(reg_res_lr, reg_wf_lr) %>% 
# linear_stack()
#' @export
fit_members <- function(model_stack, data = NULL, ...) {
  
  if (is.null(data)) {
    data <- model_stack[["train"]]
  }
  
  # pick out which submodels have nonzero coefs
  member_names <- get_glmn_coefs(model_stack[["coefs"]][["fit"]]) %>%
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
      .config = stringi::stri_replace_all_fixed(
        .config,
        c("Model", "Recipe"),
        "",
        vectorize_all = FALSE)
      ) %>%
    dplyr::mutate(
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
      train_dat = data
    )
  
  model_stack[["member_fits"]] <- 
    setNames(member_fits, member_names)
  
  model_stack_constr(model_stack)
}
