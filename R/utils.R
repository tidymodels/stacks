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
get_outcome <- function(stack) {
  if (ncol(stack) == 0) {NULL} else {colnames(stack)[1]}
}
get_hash <- function(stack) {attr(stack, "rs_hash")}
get_model_def_names <- function(stack) {attr(stack, "model_def_names")}
get_model_def_hashes <- function(stack) {attr(stack, "model_def_hashes")}

# setters
set_outcome <- function(stack, members) {
  if (!is.null(get_outcome(stack)) && 
      get_outcome(stack) != tune::.get_tune_outcome_names(members)) {
    glue_stop("The model definition you've tried to add to the stack has ",
              "outcome variable {list(tune::.get_tune_outcome_names(members))}, ",
              "while the stack's outcome variable is {get_outcome(stack)}.")
  }
  
  stack
}

set_rs_hash <- function(stack, member, name) {
  new_hash <- digest::digest(member$splits)
  
  hash_matches <- get_hash(stack) %in% c("init", new_hash)
  
  if (!hash_matches) {
    glue_stop(
      "It seems like the new member '{name}' doesn't make use ",
      "of the same resampling object as the existing members."
    )
  }
  
  attr(stack, "rs_hash") <- new_hash
  
  stack
}

# note that this function sets both the model definition names and hashes
set_model_defs <- function(stack, members, name) {
  # check to make sure that the supplied model def name
  # doesn't have the same name or hash as an existing model def
  if (name %in% attr(stack, "model_def_names")) {
    glue_stop(
      "The new model definition has the ",
      "same object name '{name}' as an existing model definition."
    )
  }
  
  new_hash <- digest::digest(members)
  existing_hashes <- get_model_def_hashes(stack)

  if (new_hash %in% existing_hashes) {
    glue_stop(
      "The new member '{name}' is the same as the existing member ",
      "'{get_model_def_names(stack)[which(existing_hashes == new_hash)]}'."
    )
  }
  
  attr(stack, "model_def_names") <- c(get_model_def_names(stack), name)
  attr(stack, "model_def_hashes") <- c(get_model_def_hashes(stack), new_hash)
  
  stack
}

# remove members helpers
rm_members_checks <- function(stack, members, obj_name) {
  if (!inherits(members, "character")) {
    glue_stop(
      "The supplied model definition to remove, {obj_name}, ",
      "has class {list(class(members))} ",
      "rather than character. Did you supply the actual model definition ", 
      "object rather than its label?"
    )
  }
  
  if (!members %in% attr(stack, "model_def_names")) {
    glue_stop(
      "The supplied model definition to remove, {obj_name}, isn't in the stack."
    )
  }
  
  stack
}

rm_members <- function(stack, name) {
  members_pos <- which(name == attr(stack, "model_def_names"))
  
  attr(stack, "model_def_hashes") <- attr(stack, "model_def_hashes")[-members_pos]
  attr(stack, "model_def_names") <- attr(stack, "model_def_names")[-members_pos]
  
  update_stack_data(
    stack,
    stack %>% dplyr::select(-dplyr::contains(name))
  )
}


# Misc. Utilities
# ------------------------------------------------------------------------

collate_member <- function(stack, members, name) {
  member_cols <-
    tune::collect_predictions(members, summarize = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(.config = if (".config" %in% names(.)) .config else "Model1") %>%
    dplyr::select(!!tune::.get_tune_outcome_names(members), .row, .pred, .config) %>%
    dplyr::mutate(
      .config = stringi::stri_replace_all_fixed(
        .config,
        c("Model", "Recipe"),
        name,
        vectorize_all = FALSE
    )) %>%
    tidyr::pivot_wider(id_cols = c(".row", !!tune::.get_tune_outcome_names(members)), 
                       names_from = ".config", 
                       values_from = ".pred") %>%
    dplyr::select(-.row) 
  
  if (nrow(stack) == 0) {
    update_stack_data(
      stack, 
      member_cols
    )
  } else {
    update_stack_data(
      stack,
      dplyr::bind_cols(
        tibble::as_tibble(stack), 
        dplyr::select(member_cols, -!!get_outcome(stack))
      )
    )
  }
}

# update the data in the stack while preserving attributes and class
update_stack_data <- function(stack, new_data) {
  attr(new_data, "rs_hash") <- attr(stack, "rs_hash")
  attr(new_data, "model_def_names") <- attr(stack, "model_def_names")
  attr(new_data, "model_def_hashes") <- attr(stack, "model_def_hashes")
  
  structure(
    new_data,
    class = c("stack", class(new_data))
  )
}
