#' @export
print.stack <- function(x, ...) {
  cat(glue::glue("# A model stack with {length(x$members)} member",
                 "{if (length(x$members) != 1) 's' else ''}",
                 "{if (length(x$members) != 0) ':' else '.'}"))
  if (length(x$members) != 0) {cat("\n")}
  
  members <- 
    purrr::map2(
      unname(x$members), 
      names(x$members), 
      function(member, name) {
        cat(glue::glue(
            "#   {name}: ", 
            "{length(unique(member$.predictions[[1]]$.config))} sub-models")
          )
        cat("\n")
      }
    )
  
  cat(glue::glue("# Outcome: {get_outcome(x)}\n"))
  cat("\n")
  
  cat(glue::glue("# Evaluated: {is_evaluated(x)}"))
  cat("\n")
}
