#' @export
print.stack <- function(x, ...) {
  n_members <- if (ncol(x[["data"]]) == 0) {0} else {ncol(x[["data"]]) - 1}
  n_model_defs <- length(x[["model_defs"]])
  outcome_name <- colnames(x[["data"]])[1]
  
  cat(glue::glue("# A stack with {n_model_defs} model definition",
                 "{if (n_model_defs != 1) 's ' else ' '}",
                 "and {n_members} member",
                 "{if (n_members != 1) 's' else ''}",
                 "{if (n_model_defs != 0) ':' else '.'}"))
  if (n_model_defs != 0) {cat("\n")}
  
  n_by_model_defs <-
    purrr::map(
      names(x[["cols_map"]]),
      function(name) {
        cat(glue::glue(
            "#   {name}: ",
            "{length(x[['cols_map']][[name]])} sub-model",
            "{if (length(x[['cols_map']][[name]]) != 1) 's' else ''}")
          )
        cat("\n")
      }
    )

  cat(glue::glue("# Outcome: {outcome_name}\n"))
}
