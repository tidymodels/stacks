#' Fit a stack ensemble
#' 
#' @inheritParams add_members
# @param n The total number of sub-models to incorporate in the stack.
# @param replace Logical—whether to sample the sub-models to incorporate
# in the stack with replacement.
# @param n_initial The number of best sub-models to initialize the stack with
# before initiating subset selection.
# @param bag_p Numeric in (0, 1]—the proportion of models in the bag at
# each iteration.
#' @return A `ensemble_stack` object---this fitted model contains the necessary
#' components to predict on new data.
#' 
#' @template note_example_data
#' 
#' @examples 
#' 
#' # initialize a stack
#' st_res <- 
#'   stacks() %>%
#'   add_members(lin_reg_res_, lin_reg_wf_) %>%
#'   add_members(svm_res_, svm_wf_) %>%
#'   add_members(spline_res_, spline_wf_) %>%
#'   stack_coefficients()
#'
#' # evaluate and build an ensemble
#' stack_ensemble(st_res, st_coefs)
#' 
# ensemble <- stacks() %>% 
# add_members(svm_res_, svm_wf_) %>% 
# add_members(lin_reg_res_, lin_reg_wf_) %>% 
# stack_coefficients()
#' @export
stack_ensemble <- function(ensemble, data) {
  # pick out which models have nonzero coefs
  members <- get_glmn_coefs(ensemble[["coefs"]][["fit"]]) %>%
    dplyr::filter(c(TRUE, coefs$estimate != 0)) %>%
    dplyr::pull(terms)
  
  # make model spec with the chosen parameters
  metrics_dict <- 
    tibble::enframe(ensemble[["model_metrics"]]) %>%
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
    )
  
  members_map <- 
    tibble::as_tibble(ensemble[["cols_map"]]) %>%
    tidyr::pivot_longer(dplyr::everything()) %>%
    dplyr::inner_join(metrics_dict, by = c("value" = ".config"))
  
  # fit each of them
  member_fits <- 
    purrr::map2(
      ensemble$model_defs[members],
      ensemble,
      generics::fit,
      data = data
    )
  
  structure(
    list(
      coefs = coefs,
      member_fits = fits
    ),
    class = c("ensemble", "list")
  )
}


fit_member <- function(member_wf, member_metrics, training) {
  
}
