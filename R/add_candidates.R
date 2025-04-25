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
#' @param candidates A (set of) model definition(s) defining candidate model
#' stack members. Should inherit from `tune_results` or `workflow_set`.
#'
#' - `tune_results`: An object outputted from [tune::tune_grid()],
#' [tune::tune_bayes()], or [tune::fit_resamples()].
#' - `workflow_set`: An object outputted from `workflowsets::workflow_map()`.
#' This approach allows for supplying multiple sets of candidate members
#' with only one call to `add_candidates`. See the "Stacking With Workflow Sets"
#' article on the [package website](https://stacks.tidymodels.org/) for example code!
#'
#' Regardless, these results must have been fitted with the `control` settings
#' `save_pred = TRUE, save_workflow = TRUE`—see the [control_stack_grid()],
#' [control_stack_bayes()], and [control_stack_resamples()]
#' documentation for helper functions.
#' @param name The label for the model definition---defaults to the name
#' of the `candidates` object. Ignored if `candidates` inherits from
#' `workflow_set`.
#' @inheritParams stacks
#'
#' @return A `data_stack` object--see [stacks()] for more details!
#'
#' @template note_example_data
#'
#' @examplesIf (stacks:::should_run_examples(suggests = "kernlab"))
#' # see the "Example Data" section above for
#' # clarification on the objects used in these examples!
#'
#' # put together a data stack using
#' # tuning results for regression models
#' reg_st <-
#'   stacks() |>
#'   add_candidates(reg_res_lr) |>
#'   add_candidates(reg_res_svm) |>
#'   add_candidates(reg_res_sp)
#'
#' reg_st
#'
#' # do the same with multinomial classification models
#' class_st <-
#'   stacks() |>
#'   add_candidates(class_res_nn) |>
#'   add_candidates(class_res_rf)
#'
#' class_st
#'
#' # ...or binomial classification models
#' log_st <-
#'   stacks() |>
#'   add_candidates(log_res_nn) |>
#'   add_candidates(log_res_rf)
#'
#' log_st
#'
#' # use custom names for each model:
#' log_st2 <-
#'   stacks() |>
#'   add_candidates(log_res_nn, name = "neural_network") |>
#'   add_candidates(log_res_rf, name = "random_forest")
#'
#' log_st2
#'
#' # these objects would likely then be
#' # passed to blend_predictions():
#' log_st2 |> blend_predictions()
#'
#' @family core verbs
#' @export
add_candidates <- function(
  data_stack,
  candidates,
  name = deparse(substitute(candidates)),
  ...
) {
  check_empty_ellipses(...)

  UseMethod("add_candidates", object = candidates)
}

# check that resamples have been fitted to the workflow_set and
# then send each to add_candidates.tune_results
#' @export
add_candidates.workflow_set <- function(
  data_stack,
  candidates,
  name = deparse(substitute(candidates)),
  ...
) {
  fitted <- purrr::map_lgl(candidates$result, inherits, "tune_results")

  if (!all(fitted)) {
    if (any(fitted)) {
      not_fitted <- candidates$wflow_id[!fitted]

      cli_warn(
        c(
          "!" = "Some elements of the supplied workflow set failed to evaluate 
               with resamples.",
          "i" = "{cli::qty(sum(fitted))}The workflow{?s/} with ID 
               {.var {not_fitted}} will be excluded from the data stack."
        ),
        class = "wf_set_partial_fit"
      )

      candidates <- candidates[fitted, ]
    } else {
      cli_abort(
        "The supplied workflow set must be fitted to resamples with 
         {.help [`workflow_map()`](workflowsets::workflow_map)} before being added to a data stack.",
        class = "wf_set_unfitted"
      )
    }
  }

  purrr::reduce2(
    append(list(data_stack), candidates$result),
    candidates$wflow_id,
    add_candidates
  )
}

#' @export
add_candidates.tune_results <- function(
  data_stack,
  candidates,
  name = deparse(substitute(candidates)),
  ...
) {
  check_add_data_stack(data_stack)
  check_candidates(candidates, name)
  col_name <- check_candidate_name(name)

  stack <-
    data_stack |>
    .set_rs_hash(candidates, name) |>
    .set_splits(candidates) |>
    .set_outcome(candidates) |>
    .set_mode_(candidates, name) |>
    .set_training_data(candidates, name) |>
    .set_model_defs_candidates(candidates, name) |>
    .set_data_candidates(candidates, name, col_name)

  if (data_stack_constr(stack)) {
    stack
  }
}

#' @export
add_candidates.default <- function(data_stack, candidates, name, ...) {
  check_add_data_stack(data_stack)

  cli_abort(
    "The second argument to {.help [`add_candidates()`](stacks::add_candidates)} should inherit from one of 
     {.help [`tune_results`](tune::tune_grid)} or 
     {.help [`workflow_set`](workflowsets::workflow_set)}, but its class 
     is {.var {class(candidates)}}."
  )
}

.set_outcome <- function(stack, candidates, call = caller_env()) {
  if (
    !.get_outcome(stack) %in%
      c("init_", tune::.get_tune_outcome_names(candidates))
  ) {
    cli_abort(
      "The model definition you've tried to add to the stack has 
       outcome variable {.var {tune::.get_tune_outcome_names(candidates)}}, 
       while the stack's outcome variable is {.var {.get_outcome(stack)}}.",
      call = call
    )
  }

  attr(stack, "outcome") <- tune::.get_tune_outcome_names(candidates)

  stack
}

# checks that the hash for the resampling object
# is appropriate and then sets it
.set_rs_hash <- function(stack, candidates, name, call = caller_env()) {
  new_hash <- tune::.get_fingerprint(candidates)

  hash_matches <- .get_rs_hash(stack) %in% c("init_", new_hash)

  if (!hash_matches) {
    cli_abort(
      "It seems like the new candidate member '{name}' doesn't make use 
       of the same resampling object as the existing candidates.",
      call = call
    )
  }

  attr(stack, "rs_hash") <- new_hash

  stack
}

# set the resamples used in the data stack --
# don't need to check the resample as it would be
# redundant with checking it's hash
.set_splits <- function(stack, candidates) {
  splits_cols <- c(
    "splits",
    colnames(candidates)[grep("id", names(candidates))]
  )

  attr(stack, "splits") <- candidates |>
    dplyr::select(dplyr::all_of(splits_cols))
  attr(attr(stack, "splits"), "rset_info") <- attr(candidates, "rset_info")

  stack
}

# note whether classification or regression
.set_mode_ <- function(stack, candidates, name, call = caller_env()) {
  wf_spec <-
    attr(candidates, "workflow") |>
    workflows::extract_spec_parsnip()

  new_mode <- wf_spec$mode
  old_mode <- attr(stack, "mode")

  if (isFALSE(new_mode %in% c("regression", "classification"))) {
    cli_abort(
      "The {.pkg stacks} package does not support stacking models with mode
       {.val {new_mode}}.",
      call = call
    )
  }

  attr(stack, "mode") <- new_mode

  stack
}

# check to make sure that the supplied model def name
# doesn't have the same name or hash as an existing model def
# and then appends the model definition, hash, and metrics
.set_model_defs_candidates <- function(
  stack,
  candidates,
  name,
  call = caller_env()
) {
  if (name %in% .get_model_def_names(stack)) {
    cli_abort(
      "The new model definition has the 
       same name '{name}' as an existing model definition.",
      call = call
    )
  }

  if (attr(stack, "mode") == "classification") {
    # check to make sure that the candidates include a prob_metric so that
    # collect_predictions won't supply only hard class predictions
    metric_types <- candidates |>
      attributes() |>
      purrr::pluck("metrics") |>
      attributes() |>
      purrr::pluck("metrics") |>
      purrr::map_chr(class_1) |>
      unname()

    if (!"prob_metric" %in% metric_types) {
      cli_abort(
        "The supplied candidates were tuned/fitted using only metrics that 
         rely on hard class predictions. Please tune/fit with at least one 
         class probability-based metric, such as {.help [`roc_auc`](yardstick::roc_auc)}.",
        call = call
      )
    }
  }

  model_defs <- attr(stack, "model_defs")
  model_metrics <- attr(stack, "model_metrics")

  model_defs[[name]] <- attr(candidates, "workflow")
  model_metrics[[name]] <- tune::collect_metrics(candidates)

  attr(stack, "model_defs") <- model_defs
  attr(stack, "model_metrics") <- model_metrics

  stack
}

class_1 <- function(.x) {
  class(.x)[[1]]
}

# checks that the training data in a newly added candidate
# is the same is that from existing candidates, and sets the
# training data if the new candidate is the first in the stack
.set_training_data <- function(stack, candidates, name, call = caller_env()) {
  training_data <- attr(stack, "train")
  new_data <- tibble::as_tibble(candidates[["splits"]][[1]][["data"]])

  if (
    (!identical(training_data, tibble::tibble())) &&
      (!identical(training_data, new_data))
  ) {
    cli_abort(
      "The newly added candidate member, `{name}`, 
       uses different training data than the existing candidates.",
      call = call
    )
  }

  attr(stack, "train") <- new_data

  stack
}

# appends assessment set predictions to a data stack
.set_data_candidates <- function(stack, candidates, name, col_name) {
  candidate_cols <-
    collate_predictions(candidates) |>
    dplyr::ungroup()

  candidate_cols <- 
    candidate_cols |>
    dplyr::mutate(
      .config = if (".config" %in% names(candidate_cols)) .config else NA_character_
    ) |>
    dplyr::select(
      !!tune::.get_tune_outcome_names(candidates),
      .row,
      dplyr::contains(".pred"),
      .config
    )
  
  candidate_cols <-
    candidate_cols |>
    dplyr::mutate(
      .config = process_.config(.config, df = candidate_cols, name = col_name)
    ) |>
    tidyr::pivot_wider(
      id_cols = c(".row", !!tune::.get_tune_outcome_names(candidates)),
      names_from = ".config",
      values_from = dplyr::contains(".pred")
    ) |>
    dplyr::select(-.row)

  if (attr(stack, "mode") == "classification") {
    candidate_cols <- remove_class_preds(candidate_cols)
  }

  if (nrow(stack) == 0) {
    stack <-
      update_stack_data(
        stack,
        candidate_cols |> rm_duplicate_cols()
      )
  } else {
    stack <-
      update_stack_data(
        stack,
        dplyr::bind_cols(
          tibble::as_tibble(stack),
          dplyr::select(candidate_cols, -!!.get_outcome(stack))
        ) |>
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
    cli_warn(
      "Predictions from {length(exclude)} candidate{?s} were identical to 
       those from existing candidates and were removed from the data stack."
    )

    df <- df |> dplyr::select(-any_of(exclude))
  }

  df
}

# update the data in the stack while preserving attributes and class
update_stack_data <- function(stack, new_data) {
  attr(new_data, "rs_hash") <- attr(stack, "rs_hash")
  attr(new_data, "outcome") <- attr(stack, "outcome")
  attr(new_data, "mode") <- attr(stack, "mode")
  attr(new_data, "model_defs") <- attr(stack, "model_defs")
  attr(new_data, "cols_map") <- attr(stack, "cols_map")
  attr(new_data, "model_metrics") <- attr(stack, "model_metrics")
  attr(new_data, "train") <- attr(stack, "train")
  attr(new_data, "splits") <- attr(stack, "splits")

  structure(
    new_data,
    class = c("data_stack", class(new_data))
  )
}

check_add_data_stack <- function(data_stack, call = caller_env()) {
  if (
    rlang::inherits_any(
      data_stack,
      c("tune_results", "tune_bayes", "resample_results")
    )
  ) {
    cli_abort(
      "It looks like the first argument inherits from {.var {class(data_stack)}} 
       rather than {.var data_stack}. 
       Did you accidentally supply the candidate members as the first argument? 
       If so, please supply the output of {.help [`stacks()`](stacks::stacks)} or another 
       {.help [`add_candidates()`](stacks::add_candidates)} call as 
       the argument to {.arg data_stack}.",
      call = call
    )
  } else {
    check_inherits(data_stack, "data_stack", call = caller_env())
  }
}

check_candidates <- function(candidates, name, call = caller_env()) {
  if (nrow(tune::collect_notes(candidates)) != 0) {
    cli_warn(
      "The inputted {.arg candidates} argument {.var {name}} generated notes during 
       tuning/resampling. Model stacking may fail due to these 
       issues; see {.help [`collect_notes()`](tune::collect_notes)} if so."
    )
  }

  if (
    (!".predictions" %in% colnames(candidates)) |
      is.null(attributes(candidates)$workflow)
  ) {
    cli_abort(
      "The inputted {.arg candidates} argument was not generated with the 
       appropriate control settings. Please see {.help [`control_stack()`](stacks::control_stack)}.",
      call = call
    )
  }
}

check_candidate_name <- function(name, call = caller_env()) {
  if (
    rlang::inherits_any(
      name,
      c("tune_results", "tune_bayes", "resample_results")
    )
  ) {
    cli_abort(
      "The inputted {.arg name} argument looks like a tuning/fitting results object 
       that might be supplied as a {.arg candidates} argument. Did you try to add 
       more than one set of candidates in one {.help [`add_candidates()`](stacks::add_candidates)} call?",
      call = call
    )
  } else {
    check_inherits(name, "character", call = caller_env())

    if (make.names(name) != name) {
      cli_inform(
        "The inputted {.arg name} argument cannot prefix a valid column name. The  
         data stack will use '{make.names(name)}' rather than '{name}' in 
         constructing candidate names."
      )
    }
  }

  make.names(name)
}

# takes in the name a .config column and outputs the
# processed version for use as a unique id
process_.config <- function(.config, df, name) {
  .config_ <- if (".config" %in% colnames(df)) {
    .config
  } else {
    NA_character_
  }

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
  res <- tune::collect_predictions(x, summarize = TRUE) |>
    dplyr::rename_with(make.names, .cols = dplyr::starts_with(".pred"))

  if (inherits(x, "tune_race")) {
    config_counts <-
      tune::collect_metrics(x, summarize = FALSE) |>
      dplyr::group_by(.config) |>
      dplyr::count() |>
      dplyr::ungroup()
    # At least one configuration will always be fully resampled. We can filter
    # on configurations that have the maximum number of resamples.
    complete_count <- max(config_counts$n, na.rm = TRUE)
    retain_configs <-
      config_counts |>
      dplyr::filter(n == complete_count) |>
      dplyr::select(.config)
    res <- dplyr::inner_join(res, retain_configs, by = ".config")
  }
  res
}

# given a set of candidate columns, removes those with hard class predictions
remove_class_preds <- function(x) {
  lvls <- make.names(paste0(".prefix.", levels(factor(x[[1]]))))
  lvls <- gsub(".prefix.", "", lvls, fixed = TRUE)

  # gather indices for the columns with class probability predictions
  prob_preds_idx <- purrr::map(
    paste0(".pred_", lvls),
    grepl,
    x = colnames(x)
  ) |>
    purrr::pmap(any) |>
    unlist()

  # select the columns that look like they have probability predictions,
  # get rid of entries that would have been okayed because of an outcome
  # level called "class", and re-attach the outcome column itself
  res <- x[, prob_preds_idx] |>
    dplyr::select(where(is.numeric))
  res <- dplyr::bind_cols(x[, 1], res)
  setNames(res, make.names(names(res)))
}
