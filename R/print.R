#' @export
print.stack <- function(stack) {
  cat(glue::glue("# A model stack with {length(stack$members)} member",
                 "{if (length(stack$members) != 1) 's' else ''}",
                 "{if (length(stack$members) != 0) ':' else '.'}"))
  if (length(stack$members) != 0) {cat("\n")}
  
  members <- 
    purrr::map2(
      unname(stack$members), 
      names(stack$members), 
      function(member, name) {
        cat(glue::glue(
            "#   {name}: ", 
            "{length(unique(member$.predictions[[1]]$.config))} sub-models")
          )
        cat("\n")
      }
    )
  
  cat(glue::glue("# Outcome: {get_outcome(stack)}\n"))
  cat("\n")
  
  cat(glue::glue("# Evaluated: {is_evaluated(stack)}"))
  cat("\n")
}
