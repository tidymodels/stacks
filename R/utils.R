# Re-exports
# ------------------------------------------------------------------------
#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

# Global Variables
# ------------------------------------------------------------------------
utils::globalVariables(c(
  "stack"
))

# Utility Functions
# ------------------------------------------------------------------------
glue_stop <- function(..., .sep = "", .envir = parent.frame()) {
  rlang::abort(glue::glue(..., .sep = .sep, .envir = .envir))
}

glue_warn <- function(..., .sep = "", .envir = parent.frame()) {
  rlang::warn(glue::glue(..., .sep = .sep, .envir = .envir))
}

glue_message <- function(..., .sep = "", .envir = parent.frame()) {
  rlang::inform(glue::glue(..., .sep = .sep, .envir = .envir))
}

chr_check <- function(x) {
  cl <- match.call()
  
  if (is.null(x)) {
    glue_stop("Element `{cl$x}` should not be NULL.")
  }
  
  if (!is.character(x)) {
    glue_stop("Element `{cl$x}` should be a character string.")
  }
  
  invisible(TRUE)
}

# more easily arranged names (from recipes)
names0 <- function(num, prefix = "x") {
  if (num < 1)
    rlang::abort("`num` should be > 0.")
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}

# Constructor Utilities
# ------------------------------------------------------------------------

# getters
get_outcome <- function(stack) {stack$outcome}

# setters
set_outcome <- function(stack, member) {
  if (!is.null(get_outcome(stack)) && 
      get_outcome(stack) != tune::outcome_names(member)) {
    glue_stop("The member you've tried to add to the stack has ",
              "outcome variable {tune::outcome_names(member)}, ",
              "while the stack's outcome variable is {get_outcome(stack)}.")
  }
  
  stack$outcome <- tune::outcome_names(member)
  
  stack
}

# checks
check_hash <- function(stack, member, name) {
  if (stack$rs_hash == "init") {stack$rs_hash <- digest::digest(member$splits)}
  
  hash_matches <- stack$rs_hash == digest::digest(member$splits)
  
  if (!hash_matches) {
    glue_stop(
      "It seems like the new member '{name}' doesn't make use ",
      "of the same resampling object as the existing members."
    )
  }
  
  stack
}

check_member_name <- function(stack, member, name) {
  # check to make sure that the supplied sub-model set (member) 
  # doesn't have the same name as an existing member
  if (name %in% names(stack$members)) {
    glue_stop(
      "The new member has the ",
      "same object name '{name}' as an existing member."
    )
  }
}

# predicates
is_evaluated <- function(stack) {!is.null(stack$coefficients)}

# Misc. Utilities
# ------------------------------------------------------------------------

get_all_preds <- function(x) {
  
  params <- attributes(x)$parameters$id
  
  pred <-
    tune::collect_predictions(x, summarize = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::select(.row, .pred, .config) %>%
    tidyr::pivot_wider(id_cols = ".row", 
                       names_from = ".config", 
                       values_from = ".pred") %>%
    dplyr::select(-.row)
  
  pred
}



