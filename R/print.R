#' @export
print.stack <- function(stack) {
  
  cat(glue::glue("# A model stack with {length(stack$members)} members:"))
  cat("\n")
  
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
