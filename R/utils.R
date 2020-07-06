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

check_inherits <- function(x, what) {
  cl <- match.call()
  
  if (!inherits(x, what)) {
    glue_stop("Element `{list(cl$x)}` needs to inherit from `{what}`, but its ",
              "class is `{list(class(x))}`.")
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
  attr(stack, "outcome")
}

get_rs_hash <- function(stack) {attr(stack, "rs_hash")}
get_model_def_names <- function(stack) {names(attr(stack, "model_defs"))}
get_model_hashes <- function(stack) {attr(stack, "model_def_hashes")}

# setters
set_outcome <- function(stack, members) {
  if (!get_outcome(stack) %in% c("init_", tune::.get_tune_outcome_names(members))) {
    glue_stop("The model definition you've tried to add to the stack has ",
              "outcome variable {list(tune::.get_tune_outcome_names(members))}, ",
              "while the stack's outcome variable is {get_outcome(stack)}.")
  }
  
  attr(stack, "outcome") <- tune::.get_tune_outcome_names(members)
  
  stack
}

set_rs_hash <- function(stack, members, name) {
  new_hash <- digest::digest(members$splits)
  
  hash_matches <- get_rs_hash(stack) %in% c("init_", new_hash)
  
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
  existing_hashes <- get_model_hashes(stack)

  if (new_hash %in% existing_hashes) {
    glue_stop(
      "The new member '{name}' is the same as the existing member ",
      "'{get_model_def_names(stack)[which(existing_hashes == new_hash)]}'."
    )
  }
  
  attr(stack, "model_hashes") <- c(get_model_hashes(stack), new_hash)
  
  stack
}

# note whether classification or regression
set_mode_ <- function(stack, members, name) {
  wf_spec <- 
    attr(members, "workflow") %>%
    workflows::pull_workflow_spec()
  
  new_mode <- wf_spec$mode
  old_mode <- attr(stack, "mode")
  
  if (!old_mode %in% c("init_", new_mode)) {
    glue_stop(
      "The current mode for the stack is {old_mode}, while the mode for the ",
      "newly added member `{name}` is {new_mode}."
    )
  }
  
  attr(stack, "mode") <- new_mode
  
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
set_model_defs_members <- function(stack, members, name) {
  model_defs <- attr(stack, "model_defs")
  model_metrics <- attr(stack, "model_metrics")
  
  model_defs[[name]] <- attr(members, "workflow") %>% stack_workflow()
  model_metrics[[name]] <- tune::collect_metrics(members)
  
  attr(stack, "model_defs") <- model_defs
  attr(stack, "model_metrics") <- model_metrics
  
  stack
}

set_training_data <- function(stack, members, name) {
  training_data <- attr(stack, "train")
  new_data <- members[["splits"]][[1]][["data"]]
  
  if ((!identical(training_data, tibble::tibble())) &&
        (!identical(training_data, new_data))) {
      glue_stop("The newly added member, `{name}`, ",
                "uses a different assessment set than the existing members.")
  }
  
  attr(stack, "train") <- new_data
  
  stack
}


set_data_members <- function(stack, members, name) {
  member_cols <-
    tune::collect_predictions(members, summarize = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      .config = if (".config" %in% names(.)) .config else "Model1"
    ) %>%
    dplyr::select(
      !!tune::.get_tune_outcome_names(members), 
      .row, 
      dplyr::contains(".pred"), 
      .config
    ) %>%
    dplyr::mutate(
      .config = stringi::stri_replace_all_fixed(
        .config,
        c("Model", "Recipe"),
        name,
        vectorize_all = FALSE
    )) %>%
    tidyr::pivot_wider(
      id_cols = c(".row", !!tune::.get_tune_outcome_names(members)), 
      names_from = ".config", 
      values_from = dplyr::contains(".pred")
    ) %>%
    dplyr::select(-.row) 
  
  pred_class_idx <- 
    stringi::stri_detect_fixed(
      colnames(member_cols), 
      ".pred_class"
    )
  
  member_cols <- member_cols[,!pred_class_idx]
  
  if (nrow(stack) == 0) {
    stack <- 
      update_stack_data(
        stack, 
        member_cols
      )
  } else {
    stack <- 
      update_stack_data(
        stack,
        dplyr::bind_cols(
          tibble::as_tibble(stack), 
          dplyr::select(member_cols, -!!get_outcome(stack))
        )
      )
  }
  
  stack <- log_resample_cols(stack, member_cols, name)
  
  stack
}

# logs which columns in the data stack came from which members
log_resample_cols <- function(stack, member_cols, name) {
  new_cols <- colnames(member_cols)
  
  cols_map <- attr(stack, "cols_map")
  cols_map[[name]] <- new_cols[2:length(new_cols)]
  attr(stack, "cols_map") <- cols_map
  
  stack
}


# update the data in the stack while preserving attributes and class
update_stack_data <- function(stack, new_data) {
  attr(new_data, "rs_hash") <- attr(stack, "rs_hash")
  attr(new_data, "outcome")  <- attr(stack, "outcome") 
  attr(new_data, "mode")  <- attr(stack, "mode") 
  attr(new_data, "model_defs")  <- attr(stack, "model_defs") 
  attr(new_data, "cols_map") <- attr(stack, "cols_map")
  attr(new_data, "model_hashes") <- attr(stack, "model_hashes") 
  attr(new_data, "model_metrics")  <- attr(stack, "model_metrics") 
  attr(new_data, "train") <- attr(stack, "train")

  structure(
    new_data,
    class = c("data_stack", class(new_data))
  )
}

get_glmn_coefs <- function(x, penalty = 0.01) {
  x <- coef(x, s = penalty)
  x <- as.matrix(x)
  colnames(x) <- "estimate"
  rn <- rownames(x)
  x <- tibble::as_tibble(x) %>% dplyr::mutate(terms = rn, penalty = penalty)
  x <- dplyr::select(x, terms, estimate, penalty)
  if (is.list(x$estimate)) {
    x$estimate <- purrr::map(x$estimate, ~ as_tibble(as.matrix(.x), rownames = "terms"))
    x <- tidyr::unnest(x, cols = c(estimate), names_repair = "minimal")
    names(x) <- c("class", "terms", "estimate", "penalty")
  }
  x
}

fit_member <- function(name, wflows, members_map, train_dat) {
  member_row <- 
    members_map %>%
    dplyr::filter(value == name)
  
  member_params <- 
    wflows[[member_row$name.x]] %>%
    dials::parameters() %>%
    dplyr::pull(id)
  
  needs_finalizing <- length(member_params) != 0
  
  if (needs_finalizing) {
    member_metrics <-
      members_map %>%
      dplyr::filter(value == name)
    
    member_wf <- 
      wflows[[member_metrics$name.x]]
    
    new_member <- 
      tune::finalize_workflow(member_wf, member_metrics[,member_params]) %>%
      generics::fit(data = train_dat)
  } else {
    member_model <-
      members_map %>%
      dplyr::filter(value == name) %>%
      dplyr::select(name.x) %>%
      dplyr::pull()
    
    new_member <-
      generics::fit(wflows[[member_model[1]]], data = train_dat)
  }
  
  new_member
}

sanitize_classification_names <- function(model_stack, member_names) {
  outcome_levels <-
    model_stack[["train"]] %>%
    dplyr::select(!!get_outcome(model_stack)) %>%
    dplyr::pull() %>%
    as.character() %>%
    unique()
  
  pred_strings <- paste0(".pred_", outcome_levels, "_")
  
  new_member_names <- 
    stringi::stri_replace_all_fixed(
      member_names,
      pred_strings,
      "",
      vectorize_all = FALSE
    )
  
  tibble::tibble(
    old = member_names,
    new = new_member_names
  )
}

# takes in a workflow and returns a minimal workflow for
# use in the stack
stack_workflow <- function(x) {
  workflows::workflow() %>%
    recipes::add_model(workflows::pull_workflow_spec(x)) %>%
    recipes::add_recipe(workflows::pull_workflow_preprocessor(x))
}

