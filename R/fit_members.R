#' Fit model stack members with non-zero stacking coefficients
#'
#' @description
#' After evaluating a data stack with [blend_predictions()],
#' some number of candidates will have nonzero stacking
#' coefficients. Such candidates are referred to as "members."
#' Since members' predictions will ultimately inform the model
#' stack's predictions, members should be trained on the full
#' training set using `fit_members()`.
#'
#' @param model_stack A `model_stack` object outputted by [blend_predictions()].
#' @inheritParams stacks
#' @inheritParams blend_predictions
#' @return A `model_stack` object with a subclass `linear_stack`---this fitted
#' model contains the necessary components to predict on new data.
#'
#' @details
#' To fit members in parallel, please create a plan with the future package.
#' See the documentation of [future::plan()] for examples.
#'
#' @template note_example_data
#'
#' @examplesIf (stacks:::should_run_examples(suggests = c("ranger", "kernlab")))
#'
#' # see the "Example Data" section above for
#' # clarification on the objects used in these examples!
#'
#' # put together a data stack
#' reg_st <-
#'   stacks() |>
#'   add_candidates(reg_res_lr) |>
#'   add_candidates(reg_res_svm) |>
#'   add_candidates(reg_res_sp)
#'
#' reg_st
#'
#' # evaluate the data stack and fit the member models
#' reg_st |>
#'   blend_predictions() |>
#'   fit_members()
#'
#' reg_st
#'
#' # do the same with multinomial classification models
#' class_st <-
#'   stacks() |>
#'   add_candidates(class_res_nn) |>
#'   add_candidates(class_res_rf) |>
#'   blend_predictions() |>
#'   fit_members()
#'
#' class_st
#'
#' # ...or binomial classification models
#' log_st <-
#'   stacks() |>
#'   add_candidates(log_res_nn) |>
#'   add_candidates(log_res_rf) |>
#'   blend_predictions() |>
#'   fit_members()
#'
#' log_st
#'
#' @family core verbs
#' @export
fit_members <- function(model_stack, ...) {
  check_model_stack(model_stack)
  check_for_required_packages(model_stack)
  check_empty_ellipses(...)

  dat <- model_stack[["train"]]

  # pick out which submodels have nonzero coefs
  member_names <-
    .get_glmn_coefs(
      model_stack[["coefs"]][["fit"]],
      model_stack[["coefs"]][["spec"]][["args"]][["penalty"]]
    )

  member_names <-
    vctrs::vec_slice(
      member_names$terms,
      member_names$estimate != 0 & member_names$terms != "(Intercept)"
    )

  if (model_stack[["mode"]] == "classification") {
    member_dict <-
      sanitize_classification_names(model_stack, member_names)

    member_names <- member_dict$new |> unique()
  }

  # make model specs with the chosen parameters
  # for chosen sub-models
  metrics_dict <-
    tibble::enframe(model_stack[["model_metrics"]]) |>
    tidyr::unnest(cols = value)
  metrics_dict <-
    metrics_dict |>
    dplyr::mutate(
      .config = process_.config(.config, metrics_dict, name = make.names(name))
    )

  if (model_stack[["mode"]] == "regression") {
    members_map <-
      tibble::enframe(model_stack[["cols_map"]]) |>
      tidyr::unnest(cols = value) |>
      dplyr::full_join(
        metrics_dict,
        by = c("value" = ".config"),
        multiple = "all"
      )
  } else {
    members_map <-
      tibble::enframe(model_stack[["cols_map"]]) |>
      tidyr::unnest(cols = value) |>
      dplyr::full_join(member_dict, by = c("value" = "old"), multiple = "all")

    members_map <- vctrs::vec_slice(members_map, !is.na(members_map$new))
    members_map <- members_map[, c("name", "new")]
    members_map$value <- members_map$new
    members_map <- vctrs::vec_slice(members_map, !duplicated(members_map$value))

    members_map <- members_map |>
      dplyr::full_join(
        metrics_dict,
        by = c("value" = ".config"),
        multiple = "all"
      )
  }

  if (uses_foreach_only()) {
    warn_foreach_deprecation()
  }

  # fit each of them
  member_fits <-
    furrr::future_map(
      member_names,
      .f = fit_member,
      wflows = model_stack[["model_defs"]],
      members_map = members_map,
      train_dat = dat,
      .options = furrr::furrr_options(seed = TRUE)
    )

  model_stack[["member_fits"]] <-
    setNames(member_fits, member_names)

  if (model_stack_constr(model_stack)) {
    model_stack
  }
}


# fit one member of the ensemble
fit_member <- function(name, wflows, members_map, train_dat) {
  member_row <- vctrs::vec_slice(members_map, members_map$value == name)

  member_params <-
    parsnip::extract_parameter_set_dials(wflows[[member_row$name.x[1]]])

  member_params <- member_params$id

  needs_finalizing <- length(member_params) != 0

  if (needs_finalizing) {
    member_metrics <-
      vctrs::vec_slice(member_row, 1L)

    member_wf <-
      wflows[[member_metrics$name.x]]

    new_member <-
      tune::finalize_workflow(member_wf, member_metrics[, member_params]) |>
      parsnip::fit(data = train_dat)
  } else {
    member_model <- member_row$name.x

    new_member <-
      parsnip::fit(wflows[[member_model[1]]], data = train_dat)
  }

  new_member
}

# creates a map for column / entry names resulting
# from tuning in the classification setting
sanitize_classification_names <- function(model_stack, member_names) {
  outcome_levels <- model_stack[["train"]]
  outcome_levels <- outcome_levels[[.get_outcome(model_stack)]]
  outcome_levels <- unique(as.character(outcome_levels))

  pred_strings <- paste0(".pred_", outcome_levels, "_") |>
    make.names()

  new_member_names <-
    gsub(
      pattern = paste0(pred_strings, collapse = "|"),
      x = member_names,
      replacement = ""
    )

  tibble::new_tibble(vctrs::df_list(
    old = member_names,
    new = new_member_names
  ))
}


check_model_stack <- function(model_stack, call = caller_env()) {
  if (inherits(model_stack, "model_stack")) {
    if (!is.null(model_stack[["member_fits"]])) {
      cli_warn(
        "The members in the supplied {.arg model_stack} have already been fitted 
         and need not be fitted again."
      )
    }

    return(invisible(TRUE))
  } else if (inherits(model_stack, "data_stack")) {
    cli_abort(
      "The supplied {.arg model_stack} argument is a data stack rather than 
       a model stack. Did you forget to first evaluate the ensemble's 
       stacking coefficients with 
      {.help [`blend_predictions()`](stacks::blend_predictions)}?",
      call = call
    )
  } else {
    check_inherits(model_stack, "model_stack", call = caller_env())
  }
}

# given a model stack, find the packages required to fit members and predict
# on new values, and error if any of them are not loaded
check_for_required_packages <- function(x) {
  # for dispatch to required_pkgs.workflow when model
  # is loaded in a fresh environment
  suppressPackageStartupMessages(requireNamespace("workflows"))

  pkgs <-
    purrr::map(
      x$model_defs,
      parsnip::required_pkgs
    ) |>
    unlist() |>
    unique()

  installed <- purrr::map_lgl(
    pkgs,
    is_installed_
  )

  if (any(!installed)) {
    error_needs_install(pkgs, installed, call = caller_env())
  }

  purrr::map(
    pkgs,
    function(.x)
      suppressPackageStartupMessages(requireNamespace(.x, quietly = TRUE))
  )

  invisible(TRUE)
}

# takes in a vector of package names and a logical vector giving
# whether or not each is installed
error_needs_install <- function(pkgs, installed, call) {
  need_install <- pkgs[!installed]

  cli_abort(
    "{cli::qty(need_install)}The package{?s} {.pkg {need_install}} need{?s/} to be 
     installed before fitting members.",
    call = call
  )
}

is_installed_ <- function(pkg) {
  rlang::is_installed(pkg)
}

uses_foreach_only <- function() {
  future::nbrOfWorkers() == 1 && foreach::getDoParWorkers() > 1
}

warn_foreach_deprecation <- function() {
  cli::cli_warn(c(
    "!" = "{.pkg stacks} detected a parallel backend registered with \\
           foreach but no backend registered with future.",
    "i" = "Support for parallel processing with foreach was \\
           deprecated in {.pkg stacks} 1.0.6.",
    "i" = "See {.help tune::parallelism} to learn more."
  ))
}
