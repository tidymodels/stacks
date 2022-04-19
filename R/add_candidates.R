#' Add model definitions to a data stack
#'
#' @description 
#' `add_candidates()` collates the assessment set predictions
#' and additional attributes from the supplied model definition
#' (i.e. set of "candidates") to a data stack. 
#' 
#' Behind the scenes, data stack objects are just [tibble::tbl_df]s,
#' where the first column gives the true response values,
#' and the remaining columns give the assessment set predictions
#' for each candidate. In the regression setting, there's only 
#' one column per ensemble member. In classification settings, 
#' there are as many columns per candidate ensemble member 
#' as there are levels of the outcome variable.
#' 
#' To initialize a data stack, use the `stacks()` function.
#' Model definitions are appended to a data stack iteratively 
#' using several calls to `add_candidates()`. Data stacks are 
#' evaluated using the [blend_predictions()] function.
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
#'   add_candidates(reg_res_lr) %>%
#'   add_candidates(reg_res_svm) %>%
#'   add_candidates(reg_res_sp)
#'   
#' reg_st
#'   
#' # do the same with multinomial classification models
#' class_st <-
#'   stacks() %>%
#'   add_candidates(class_res_nn) %>%
#'   add_candidates(class_res_rf)
#'   
#' class_st
#'   
#' # ...or binomial classification models
#' log_st <-
#'   stacks() %>%
#'   add_candidates(log_res_nn) %>%
#'   add_candidates(log_res_rf)
#'   
#' log_st
#'   
#' # use custom names for each model:
#' log_st2 <-
#'   stacks() %>%
#'   add_candidates(log_res_nn, name = "neural_network") %>%
#'   add_candidates(log_res_rf, name = "random_forest")
#'   
#' log_st2
#'   
#' # these objects would likely then be
#' # passed to blend_predictions():
#' log_st2 %>% blend_predictions()
#' }
#' 
#' @family core verbs
#' @export
add_candidates <- function(data_stack, candidates,
                           name = deparse(substitute(candidates)), ...) {
  UseMethod("add_candidates", object = candidates)
}

# check that resamples have been fitted to the workflow_set and
# then send each to add_candidates.tune_results
#' @export
add_candidates.workflow_set <- function(data_stack, candidates, 
                                        name = deparse(substitute(candidates)), 
                                        ...) {
  if (!"result" %in% colnames(candidates)) {
    glue_stop(
      "The supplied workflow_set must be fitted to resamples with ",
      "workflows::workflow_map() before being added to a data stack."
    )
  }
  
  purrr::reduce2(
    append(list(data_stack), candidates$result),
    candidates$wflow_id,
    add_candidates
  )
}

#' @export
add_candidates.tune_results <- function(data_stack, candidates, 
                                        name = deparse(substitute(candidates)), 
                                        ...) {
  check_add_data_stack(data_stack)
  check_candidates(candidates, name)
  col_name <- check_name(name)
  
  stack <- 
    data_stack %>%
    .set_rs_hash(candidates, name) %>%
    .set_splits(candidates) %>%
    .set_outcome(candidates) %>%
    .set_mode_(candidates, name) %>%
    .set_training_data(candidates, name) %>%
    .set_model_defs_candidates(candidates, name) %>%
    .set_data_candidates(candidates, name, col_name)
  
  if (data_stack_constr(stack)) {stack}
}

#' @export
add_candidates.default <- function(data_stack, candidates, name, ...) {
  check_add_data_stack(data_stack)
  
  glue_stop(
    "The second argument to add_candidates() should inherit from one of ",
    "`tune_results` or `workflow_set`, but its class ",
    "is {list(class(candidates))}."
  )
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
  new_hash <- tune::.get_fingerprint(candidates)
  
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
  splits_cols <- c("splits", colnames(candidates)[grep("id", names(candidates))])
  
  attr(stack, "splits") <- candidates %>% dplyr::select(dplyr::all_of(splits_cols))
  attr(attr(stack, "splits"), "rset_info") <- attr(candidates, "rset_info")
  
  stack
}

# note whether classification or regression
.set_mode_ <- function(stack, candidates, name) {
  wf_spec <- 
    attr(candidates, "workflow") %>%
    workflows::extract_spec_parsnip()
  
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
  
  if (attr(stack, "mode") == "classification") {
    # check to make sure that the candidates include a prob_metric so that
    # collect_predictions won't supply only hard class predictions
    metric_types <- candidates %>%
      attributes() %>%
      purrr::pluck("metrics") %>%
      attributes() %>%
      purrr::pluck("metrics") %>%
      purrr::map_chr(~class(.x)[[1]]) %>%
      unname()
    
    if (!"prob_metric" %in% metric_types) {
      glue_stop(
        "The supplied candidates were tuned/fitted using only metrics that ",
        "rely on hard class predictions. Please tune/fit with at least one ",
        "class probability-based metric, such as `yardstick::roc_auc()`."        
      )
    }
  }
  
  model_defs <- attr(stack, "model_defs")
  model_metrics <- attr(stack, "model_metrics")
  
  model_defs[[name]] <- attr(candidates, "workflow") %>% stack_workflow()
  model_metrics[[name]] <- tune::collect_metrics(candidates)
  
  attr(stack, "model_defs") <- model_defs
  attr(stack, "model_metrics") <- model_metrics
  
  stack
}

# checks that the training data in a newly added candidate
# is the same is that from existing candidates, and sets the
# training data if the new candidate is the first in the stack
.set_training_data <- function(stack, candidates, name) {
  training_data <- attr(stack, "train")
  new_data <- tibble::as_tibble(candidates[["splits"]][[1]][["data"]])
  
  if ((!identical(training_data, tibble::tibble())) &&
      (!identical(training_data, new_data))) {
    glue_stop("The newly added candidate member, `{name}`, ",
              "uses different training data than the existing candidates.")
  }
  
  attr(stack, "train") <- new_data
  
  stack
}

# appends assessment set predictions to a data stack
.set_data_candidates <- function(stack, candidates, name, col_name) {
  candidate_cols <-
    collate_predictions(candidates) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      .config = if (".config" %in% names(.)) .config else NA_character_
    ) %>%
    dplyr::select(
      !!tune::.get_tune_outcome_names(candidates), 
      .row, 
      dplyr::contains(".pred"), 
      .config
    ) %>%
    dplyr::mutate(
      .config = process_.config(.config, df = ., name = col_name)
    ) %>%
    tidyr::pivot_wider(
      id_cols = c(".row", !!tune::.get_tune_outcome_names(candidates)), 
      names_from = ".config", 
      values_from = dplyr::contains(".pred")
    ) %>%
    dplyr::select(-.row) 
  
  pred_class_idx <- grepl(pattern = ".pred_class", x = colnames(candidate_cols))
  
  candidate_cols <- candidate_cols[,!pred_class_idx] %>% 
    setNames(., make.names(names(.)))
  
  if (nrow(stack) == 0) {
    stack <- 
      update_stack_data(
        stack, 
        candidate_cols %>% rm_duplicate_cols()
      )
  } else {
    stack <- 
      update_stack_data(
        stack,
        dplyr::bind_cols(
          tibble::as_tibble(stack), 
          dplyr::select(candidate_cols, -!!.get_outcome(stack))
        ) %>%
          rm_duplicate_cols()
      )
  }
  
  stack <- log_resample_cols(stack, candidate_cols, name)
  
  stack
}

# logs which columns in the data stack came from which candidates
log_resample_cols <- function(stack, candidate_cols, name) {
  new_cols <- 
    colnames(candidate_cols)[colnames(candidate_cols) %in% colnames(stack)]
  
  cols_map <- attr(stack, "cols_map")
  cols_map[[name]] <- new_cols[new_cols != attributes(stack)$outcome]
  attr(stack, "cols_map") <- cols_map
  
  stack
}

# warns if candidate columns are perfectly collinear with existing columns
rm_duplicate_cols <- function(df) {
  exclude <- character(0)
  exclude <- c(exclude, names(df[duplicated(purrr::map(df, c))]))
  
  if (length(exclude) > 0) {
    if (length(exclude) > 1) {
      n_candidates <- paste(length(exclude), "candidates")
    } else {
      n_candidates <- "1 candidate"
    }
    
    glue_warn(
      "Predictions from {n_candidates} were identical to ",
      "those from existing candidates and were removed from the data stack."
    )
    
    df <- df %>% dplyr::select(-exclude)
  }
  
  df
}

# update the data in the stack while preserving attributes and class
update_stack_data <- function(stack, new_data) {
  attr(new_data, "rs_hash") <- attr(stack, "rs_hash")
  attr(new_data, "outcome")  <- attr(stack, "outcome") 
  attr(new_data, "mode")  <- attr(stack, "mode") 
  attr(new_data, "model_defs")  <- attr(stack, "model_defs") 
  attr(new_data, "cols_map") <- attr(stack, "cols_map")
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
    workflows::add_model(workflows::extract_spec_parsnip(x))
  
  pre <- workflows::extract_preprocessor(x)
  
  if (inherits(pre, "formula")) {
    res <- res %>% workflows::add_formula(pre)
  } else if (inherits(pre, "recipe")) {
    res <- res %>% workflows::add_recipe(pre)
  } else if (inherits(pre, "workflow_variables")) {
    res <- res %>% workflows::add_variables(variables = pre)
  } else {
    rlang::abort(paste0("Can't add a preprocessor of class '", class(pre)[1], "'"))
  }
  
  res
}

check_add_data_stack <- function(data_stack) {
  if (rlang::inherits_any(
    data_stack, 
    c("tune_results", "tune_bayes", "resample_results")
  )) {
    glue_stop(
      "It looks like the first argument inherits from {list(class(data_stack))} ",
      "rather than `data_stack`. ",
      "Did you accidentally supply the candidate members as the first argument? ",
      "If so, please supply the output of `stacks()` or another `add_candidates()` as ",
      "the argument to `data_stack`."
    )
  } else {
    check_inherits(data_stack, "data_stack")
  }
}

check_candidates <- function(candidates, name) {
  if (!rlang::inherits_any(
    candidates, 
    c("tune_results", "tune_bayes", "resample_results")
  )) {
    glue_stop(
      "The inputted `candidates` argument has class `{list(class(candidates))}`",
      ", but it should inherit from one of 'tune_results', 'tune_bayes', ",
      "or 'resample_results'."
    )
  }
  
  if (nrow(tune::collect_notes(candidates)) != 0) {
    glue_warn(
      "The inputted `candidates` argument `{name}` generated notes during ",
      "tuning/resampling. Model stacking may fail due to these ",
      "issues; see `?collect_notes` if so."
    )
  }
  
  if ((!".predictions" %in% colnames(candidates)) | 
      is.null(attributes(candidates)$workflow)) {
    glue_stop(
      "The inputted `candidates` argument was not generated with the ",
      "appropriate control settings. Please see ?control_stack."
    )
  }
}

check_name <- function(name) {
  if (rlang::inherits_any(
    name, 
    c("tune_results", "tune_bayes", "resample_results")
  )) {
    glue_stop(
      "The inputted `name` argument looks like a tuning/fitting results object ",
      "that might be supplied as a `candidates` argument. Did you try to add ",
      "more than one set of candidates in one `add_candidates()` call?"
    )
  } else {
    check_inherits(name, "character")
    
    if (make.names(name) != name) {
      glue_message(
        "The inputted `name` argument cannot prefix a valid column name. The ", 
        'data stack will use "{make.names(name)}" rather than "{name}" in ',
        "constructing candidate names."
      )
    }
  }
  
  make.names(name)
}

# takes in the name a .config column and outputs the
# processed version for use as a unique id
process_.config <- function(.config, df, name) {
  .config_ <- if (".config" %in% colnames(df)) {.config} else {NA_character_}
  
  .config_ <-
    gsub(
      pattern = c("Model|Recipe"),
      replacement = "",
      x = .config_,
    )
  
  .config_ <-
    gsub(
      pattern = c("Preprocessor"),
      replacement = "_",
      x = .config_,
    )
  
  .config_ <- 
    dplyr::case_when(
      !is.na(.config_) ~ paste0(name, .config_),
      TRUE ~ paste0(name, "_1")
    )
  
  .config_
}

# For racing, we only want to keep the candidates with complete resamples. 
collate_predictions <- function(x) {
  res <- tune::collect_predictions(x, summarize = TRUE) %>%
    dplyr::rename_with(make.names, .cols = dplyr::starts_with(".pred"))
    
  if (inherits(x, "tune_race")) {
    config_counts <- 
      tune::collect_metrics(x, summarize = FALSE) %>% 
      dplyr::group_by(.config) %>% 
      dplyr::count() %>% 
      dplyr::ungroup()
    # At least one configuration will always be fully resampled. We can filter
    # on configurations that have the maximum number of resamples. 
    complete_count <- max(config_counts$n, na.rm = TRUE)
    retain_configs <- 
      config_counts %>% 
      dplyr::filter(n == complete_count) %>% 
      dplyr::select(.config)
    res <- dplyr::inner_join(res, retain_configs, by = ".config")
  }
  res
}
