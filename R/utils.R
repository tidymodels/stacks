# Re-exports
# ------------------------------------------------------------------------
#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

# Global Variables
# ------------------------------------------------------------------------
utils::globalVariables(c(
  ":=",
  ".",
  ".config",
  ".pred",
  "as.formula",
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

check_chr <- function(x) {
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

check_member_add <- function(stack, member, name) {
  # check to make sure that the supplied sub-model set (member) 
  # doesn't have the same name as an existing member
  if (name %in% names(stack$members)) {
    glue_stop(
      "The new member has the ",
      "same object name '{name}' as an existing member."
    )
  }
  
  new_member_hash <- digest::digest(member)
  existing_hashes <- purrr::map(stack$members, digest::digest)
  
  if (new_member_hash %in% existing_hashes) {
    glue_stop(
      "The new member '{name}' is the same as the existing member ",
      "'{names(stack$members)[which(existing_hashes %in% new_member_hash)]}'."
    )
  }
}

check_member_rm <- function(stack, member, name) {
  # check to make sure that the member to remove is a character
  if (!inherits(member, "character")) {
    glue_stop(
      "The supplied member to remove, {name}, has class {list(class(member))} ",
      "rather than character. Did you supply the actual member object rather ",
      "than its label?"
    )
  }
  
  if (!member %in% names(stack$members)) {
    glue_stop(
      "The supplied member to remove, {name}, isn't a stack member."
    )
  }

}


check_evaluated <- function(stack, name, context) {
  stack_is_evaluated <- is_evaluated(stack)
  
  if (stack_is_evaluated) {
    glue_warn(
      "The supplied model stack is already evaluated, and will ",
      "need to be re-evaluated with the ",
      if (context == "add") {
        "new member '{name}' added. "
      } else {
        "member '{name}' removed. "
      },
      "To silence this warning, first unevaluate the stack ",
      "with stack_uneval().")
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


