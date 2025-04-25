#' Collect candidate parameters and stacking coefficients
#'
#' @description
#' A function to help situate candidates within a stack. Takes in a data
#' stack or model stack and candidate name and returns a tibble mapping the
#' candidate/member names to their hyperparameters (and, if a model stack,
#' to their stacking coefficients as well).
#'
#' @param stack A `data_stack` or `model_stack` object.
#' @param candidates The name of the candidates to collect parameters on.
#' This will either be the `name` argument supplied to [add_candidates()] or,
#' if not supplied, the name of the object supplied to the `candidates`
#' argument in [add_candidates()].
#' @inheritParams stacks
#'
#' @return A [tibble::tbl_df] with information on member names and hyperparameters.
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
#'   add_candidates(reg_res_sp, "spline")
#'
#' reg_st
#'
#' # check out the hyperparameters for some of the candidates
#' collect_parameters(reg_st, "reg_res_svm")
#'
#' collect_parameters(reg_st, "spline")
#'
#' # blend the data stack to view the hyperparameters
#' # along with the stacking coefficients!
#' collect_parameters(
#'   reg_st |> blend_predictions(),
#'   "spline"
#' )
#' @export
collect_parameters <- function(stack, candidates, ...) {
  UseMethod("collect_parameters", stack)
}

#' @export
#' @rdname collect_parameters
collect_parameters.default <- function(stack, candidates, ...) {
  cli_abort(
    "There is no `collect_parameters()` method currently implemented  
     for {.var {class(stack)}} objects."
  )
}

#' @export
#' @rdname collect_parameters
collect_parameters.data_stack <- function(stack, candidates, ...) {
  collect_params(
    attributes(stack)$cols_map,
    attributes(stack)$model_metrics,
    candidates,
    attributes(stack)$model_defs,
    stack = stack
  )
}

#' @export
#' @rdname collect_parameters
collect_parameters.model_stack <- function(stack, candidates, ...) {
  collect_params(
    stack$cols_map,
    stack$model_metrics,
    candidates,
    stack$model_defs,
    stack$coefs,
    stack = stack
  )
}

collect_params <- function(
  cols_map,
  model_metrics,
  candidates,
  workflows,
  blend = NULL,
  stack
) {
  check_for_candidates(model_metrics, candidates)

  params <-
    workflows[[candidates]] |>
    parsnip::extract_parameter_set_dials() |>
    dplyr::pull(id)

  res <-
    model_metrics[[candidates]] |>
    dplyr::mutate(
      .config = process_.config(.config, model_metrics[[candidates]], name = candidates),
      member = gsub(
        pattern = c("Model|Recipe"),
        replacement = candidates,
        x = .config
      )
    ) |>
    dplyr::select(member, dplyr::all_of(params))
  res <- dplyr::filter(res, !duplicated(res))

  if (!is.null(blend)) {
    stacking_coefs <-
      .get_glmn_coefs(
        blend$fit,
        blend$spec$args$penalty
      ) |>
      dplyr::select(-penalty) |>
      dplyr::rename(coef = estimate)

    if (grepl(".pred", stacking_coefs$terms[2], fixed = TRUE)) {
      # classification context
      if ("class" %in% colnames(stacking_coefs)) {
        pred_strings <-
          paste(
            ".pred_",
            unique(stacking_coefs$class),
            "_",
            sep = "",
            collapse = "|"
          )
      } else {
        lvls <- levels(stack[["data_stack"]][[stack[["outcome"]]]])

        pred_strings <-
          paste(".pred_", lvls, "_", sep = "", collapse = "|")
      }

      stacking_coefs <-
        stacking_coefs |>
        dplyr::mutate(
          member = gsub(
            pred_strings,
            "",
            terms
          )
        ) |>
        dplyr::filter(member %in% res$member)

      res <-
        dplyr::full_join(
          res,
          stacking_coefs,
          by = "member",
          multiple = "all"
        ) |>
        dplyr::filter(!is.na(terms))
    } else {
      # regression context
      res <-
        dplyr::left_join(
          res,
          stacking_coefs,
          by = c("member" = "terms"),
          multiple = "all"
        )
    }
  }

  res
}

check_for_candidates <- function(
  model_metrics,
  candidates,
  call = caller_env()
) {
  if (
    (!inherits(candidates, "character")) ||
      (!candidates %in% names(model_metrics))
  ) {
    cli_abort(
      "The `candidates` argument to `collect_parameters()` must be the name 
       given to a set of candidates added with `add_candidates()`.",
      call = call
    )
  }
}
