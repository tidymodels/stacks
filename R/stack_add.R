#' Add model definitions to a data stack
#'
#' Collates assessment set predictions and appends workflows and additional 
#' attributes to a `data_stack`.
#'
#' @param data_stack A `data_stack` object.
#' @param candidates A model definition: either a `tune_results` 
#' or `resample_results` object outputted from
#' [tune::tune_grid()], [tune::tune_bayes()], or [tune::fit_resamples()].
#' These results must have been fitted with the `control` settings
#' `save_pred = TRUE, save_workflow = TRUE`—see the [control_stack_grid()],
#' [control_stack_bayes()], and [control_stack_resamples()]
#' documentation for helper functions.
#' @param name The label for the model definition---defaults to the name
#' of the `candidates` object.
#' @inheritParams stacks
#' 
#' @return A `data_stack` object--see [stacks()] for more details! 
#' 
#' @template note_example_data
#' 
#' @examples 
#' \donttest{
#' # see the "Example Data" section above for
#' # clarification on the objects used in these examples!
#' 
#' # put together a data stack using
#' # tuning results for regression models
#' reg_st <- 
#'   stacks() %>%
#'   stack_add(reg_res_lr) %>%
#'   stack_add(reg_res_svm) %>%
#'   stack_add(reg_res_sp)
#'   
#' reg_st
#'   
#' # do the same with multinomial classification models
#' class_st <-
#'   stacks() %>%
#'   stack_add(class_res_nn) %>%
#'   stack_add(class_res_rf)
#'   
#' class_st
#'   
#' # ...or binomial classification models
#' log_st <-
#'   stacks() %>%
#'   stack_add(log_res_nn) %>%
#'   stack_add(log_res_rf)
#'   
#' log_st
#'   
#' # use custom names for each model:
#' log_st2 <-
#'   stacks() %>%
#'   stack_add(log_res_nn, name = "neural_network") %>%
#'   stack_add(log_res_rf, name = "random_forest")
#'   
#' log_st2
#'   
#' # these objects would likely then be
#' # passed to stack_blend():
#' log_st2 %>% stack_blend()
#' }
#' 
#' @family core verbs
#' @export
stack_add <- function(data_stack, candidates,
                           name = deparse(substitute(candidates)), ...) {
  check_chr(name)
  
  stack <- 
    data_stack %>%
    .set_rs_hash(candidates, name) %>%
    .set_splits(candidates) %>%
    .set_outcome(candidates) %>%
    .set_mode_(candidates, name) %>%
    .set_training_data(candidates, name) %>%
    .set_model_defs_candidates(candidates, name) %>%
    .set_data_candidates(candidates, name)
  
  if (data_stack_constr(stack)) {stack}
}

.set_outcome <- function(stack, candidates) {
  if (!.get_outcome(stack) %in% c("init_", tune::.get_tune_outcome_names(candidates))) {
    glue_stop("The model definition you've tried to add to the stack has ",
              "outcome variable {list(tune::.get_tune_outcome_names(candidates))}, ",
              "while the stack's outcome variable is {.get_outcome(stack)}.")
  }
  
  attr(stack, "outcome") <- tune::.get_tune_outcome_names(candidates)
  
  stack
}

# checks that the hash for the resampling object
# is appropriate and then sets it
.set_rs_hash <- function(stack, candidates, name) {
  new_hash <- digest::digest(candidates$splits)
  
  hash_matches <- .get_rs_hash(stack) %in% c("init_", new_hash)
  
  if (!hash_matches) {
    glue_stop(
      "It seems like the new candidate member '{name}' doesn't make use ",
      "of the same resampling object as the existing candidates."
    )
  }
  
  attr(stack, "rs_hash") <- new_hash
  
  stack
}

# set the resamples used in the data stack --
# don't need to check the resample as it would be
# redundant with checking it's hash
.set_splits <- function(stack, candidates) {
  attr(stack, "splits") <- candidates %>% dplyr::select(splits, id)
  attr(attr(stack, "splits"), "rset_info") <- attr(candidates, "rset_info")
  
  stack
}

# note whether classification or regression
.set_mode_ <- function(stack, candidates, name) {
  wf_spec <- 
    attr(candidates, "workflow") %>%
    workflows::pull_workflow_spec()
  
  new_mode <- wf_spec$mode
  old_mode <- attr(stack, "mode")
  
  attr(stack, "mode") <- new_mode
  
  stack
}

# check to make sure that the supplied model def name
# doesn't have the same name or hash as an existing model def
# and then appends the model definition, hash, and metrics
.set_model_defs_candidates <- function(stack, candidates, name) {
  if (name %in% .get_model_def_names(stack)) {
    glue_stop(
      "The new model definition has the ",
      "same name '{name}' as an existing model definition."
    )
  }
  
  new_hash <- digest::digest(candidates)
  existing_hashes <- .get_model_hashes(stack)
  
  if (new_hash %in% existing_hashes) {
    glue_stop(
      "The new candidate member '{name}' is the same as the existing candidate ",
      "'{.get_model_def_names(stack)[which(existing_hashes == new_hash)]}'."
    )
  }
  
  model_defs <- attr(stack, "model_defs")
  model_metrics <- attr(stack, "model_metrics")
  
  model_defs[[name]] <- attr(candidates, "workflow") %>% stack_workflow()
  model_metrics[[name]] <- tune::collect_metrics(candidates)
  
  attr(stack, "model_defs") <- model_defs
  attr(stack, "model_metrics") <- model_metrics
  attr(stack, "model_hashes") <- c(.get_model_hashes(stack), new_hash)
  
  stack
}

# checks that the training data in a newly added candidate
# is the same is that from existing candidates, and sets the
# training data if the new candidate is the first in the stack
.set_training_data <- function(stack, candidates, name) {
  training_data <- attr(stack, "train")
  new_data <- candidates[["splits"]][[1]][["data"]]
  
  if ((!identical(training_data, tibble::tibble())) &&
      (!identical(training_data, new_data))) {
    glue_stop("The newly added candidate member, `{name}`, ",
              "uses different training data than the existing candidates.")
  }
  
  attr(stack, "train") <- new_data
  
  stack
}

# appends assessment set predictions to a data stack
.set_data_candidates <- function(stack, candidates, name) {
  candidate_cols <-
    tune::collect_predictions(candidates, summarize = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      .config = if (".config" %in% names(.)) .config else "Model1"
    ) %>%
    dplyr::select(
      !!tune::.get_tune_outcome_names(candidates), 
      .row, 
      dplyr::contains(".pred"), 
      .config
    ) %>%
    dplyr::mutate(
      .config = gsub(
        pattern = c("Model|Recipe"),
        replacement = name,
        x = .config,
      )) %>%
    tidyr::pivot_wider(
      id_cols = c(".row", !!tune::.get_tune_outcome_names(candidates)), 
      names_from = ".config", 
      values_from = dplyr::contains(".pred")
    ) %>%
    dplyr::select(-.row) 
  
  pred_class_idx <- grepl(pattern = ".pred_class", x = colnames(candidate_cols))
  
  candidate_cols <- candidate_cols[,!pred_class_idx]
  
  if (nrow(stack) == 0) {
    stack <- 
      update_stack_data(
        stack, 
        candidate_cols
      )
  } else {
    stack <- 
      update_stack_data(
        stack,
        dplyr::bind_cols(
          tibble::as_tibble(stack), 
          dplyr::select(candidate_cols, -!!.get_outcome(stack))
        )
      )
  }
  
  stack <- log_resample_cols(stack, candidate_cols, name)
  
  stack
}

# logs which columns in the data stack came from which candidates
log_resample_cols <- function(stack, candidate_cols, name) {
  new_cols <- colnames(candidate_cols)
  
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
  attr(new_data, "splits") <- attr(stack, "splits")
  
  structure(
    new_data,
    class = c("data_stack", class(new_data))
  )
}

# takes in a workflow and returns a minimal workflow for
# use in the stack
stack_workflow <- function(x) {
  res <-
    workflows::workflow() %>%
    workflows::add_model(workflows::pull_workflow_spec(x))
  
  pre <- workflows::pull_workflow_preprocessor(x)
  
  if (inherits(pre, "formula")) {
    res <- res %>% workflows::add_formula(pre)
  } else {
    res <- res %>% workflows::add_recipe(pre)
  }
  
  res
}