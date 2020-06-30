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
#' # initialize a linear model stack
#' st_res <- 
#'   stacks() %>%
#'   add_members(lin_reg_res_, lin_reg_wf_) %>%
#'   add_members(svm_res_, svm_wf_) %>%
#'   add_members(spline_res_, spline_wf_) %>%
#'   linear_stack()
#'
#' # fit the member models
#' fit_members(st_res, st_coefs)
#' 
# ensemble <- stacks() %>% 
# add_members(svm_res_, svm_wf_) %>% 
# add_members(lin_reg_res_, lin_reg_wf_) %>% 
# linear_stack()
#' @export
fit_members <- function(model_stack, data = NULL, ...) {
  
  if (is.null(data)) {
    data <- model_stack[["train"]]
  }
  
  # pick out which submodels have nonzero coefs
  member_names <- get_glmn_coefs(model_stack[["coefs"]][["fit"]]) %>%
    dplyr::filter(estimate != 0) %>%
    dplyr::pull(terms)
  
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
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(.metric == "rmse")
  
  members_map <- 
    tibble::as_tibble(model_stack[["cols_map"]]) %>%
    tidyr::pivot_longer(dplyr::everything()) %>%
    dplyr::full_join(metrics_dict, by = c("value" = ".config"))
  
  # fit each of them
  member_fits <- 
    purrr::map(
      member_names[c(member_names != "(Intercept)")],
      fit_member,
      wflows = model_stack[["model_defs"]],
      members_map = members_map,
      train_dat = data
    )
  
  model_stack[["member_fits"]] <- 
    setNames(member_fits, member_names[2:length(member_names)])
  
  model_stack_constr(model_stack)
}
