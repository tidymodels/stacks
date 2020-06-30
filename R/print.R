#' @export
print.data_stack <- function(x, ...) {
  n_members <- if (ncol(x) == 0) {0} else {ncol(x) - 1}
  n_model_defs <- length(attr(x, "model_defs"))
  outcome_name <- colnames(x)[1]
  
  cat(glue::glue("# A data stack with {n_model_defs} model definition",
                 "{if (n_model_defs != 1) 's' else ''}",
                 " and {n_members} potential member",
                 "{if (n_members != 1) 's' else ''}",
                 "{if (n_model_defs != 0) ':' else '.'}"))
  if (n_model_defs != 0) {cat("\n")}
  
  n_by_model_defs <-
    purrr::map2(
      attr(x, "cols_map"),
      names(attr(x, "cols_map")),
      function(submodels, name) {
        cat(glue::glue(
          "#   {name}: ",
          "{length(submodels)} sub-model",
          "{if (length(submodels) != 1) 's' else ''}")
        )
        cat("\n")
      }
    )
  
  cat(glue::glue("# Outcome: {get_outcome(x)}\n"))
}

#' @export
print.model_stack <- function(x, n = 10, ...) {
  outcome_name <- x[["outcome"]]
  
  member_weights <- get_glmn_coefs(x[["coefs"]][["fit"]]) %>%
    dplyr::filter(estimate != 0) %>%
    dplyr::select(term = terms, estimate)
  
  cat(glue::glue("# A model stack with {nrow(member_weights)} member",
                 "{if (nrow(member_weights) != 1) 's.' else '.'}"))
  cat("\n")
  
  print(member_weights, n = min(n, nrow(member_weights)))
}
