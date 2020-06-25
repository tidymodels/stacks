#' @export
print.stack <- function(x, ...) {
  n_members <- if (ncol(x) == 0) {0} else {ncol(x) - 1}
  n_model_defs <- length(attr(x, "model_def_names"))
  outcome_name <- colnames(x)[1]
  
  cat(glue::glue("# A model stack with {n_model_defs} model definition",
                 "{if (n_model_defs != 1) 's ' else ' '}",
                 "and {n_members} member",
                 "{if (n_members != 1) 's' else ''}",
                 "{if (n_model_defs != 0) ':' else '.'}"))
  if (n_model_defs != 0) {cat("\n")}
  
  n_by_model_defs <-
    purrr::map(
      attr(x, "model_def_names"),
      function(name) {
        cat(glue::glue(
            "#   {name}: ",
            "{sum(stringi::stri_detect_fixed(colnames(x), name))} sub-models")
          )
        cat("\n")
      }
    )

  cat(glue::glue("# Outcome: {get_outcome(x)}\n"))
}
