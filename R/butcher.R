#' @importFrom butcher butcher
#' @export
butcher::butcher

#' Axing a model_stack.
#'
#' @param x A model object
#' @param verbose Print information each time an axe method is executed.
#'   Notes how much memory is released and what functions are disabled.
#'   Default is `FALSE`.
#' @inheritParams stacks
#'
#' @return Axed model_stack object.
#'
#' @examplesIf FALSE
# (stacks:::should_run_examples(suggests = c("ranger", "kernlab")))
#'
#' # build a regression model stack
#' st <-
#'   stacks() |>
#'   add_candidates(reg_res_lr) |>
#'   add_candidates(reg_res_sp) |>
#'   blend_predictions() |>
#'   fit_members()
#'
#' # remove any of the "butcherable"
#' # elements individually
#' axe_call(st)
#' axe_ctrl(st)
#' axe_data(st)
#' axe_fitted(st)
#' axe_env(st)
#'
#' # or do it all at once!
#' butchered_st <- butcher(st, verbose = TRUE)
#'
#' format(object.size(st))
#' format(object.size(butchered_st))
#' @name axe_model_stack
NULL

#' @importFrom butcher axe_call
#' @export
butcher::axe_call

#' Remove the call.
#'
#' @rdname axe_model_stack
#' @importFrom butcher axe_call
#' @method axe_call model_stack
#' @export axe_call.model_stack
#' @export
axe_call.model_stack <- function(x, verbose = FALSE, ...) {
  res <- process_component_models(x, butcher::axe_call)

  add_butcher_attributes(
    res,
    x,
    disabled = c("print()", "summary()"),
    add_class = TRUE,
    verbose = verbose
  )
}

#' @importFrom butcher axe_ctrl
#' @export
butcher::axe_ctrl

#' Remove controls used for training.
#'
#' @rdname axe_model_stack
#' @importFrom butcher axe_ctrl
#' @method axe_ctrl model_stack
#' @export axe_ctrl.model_stack
#' @export
axe_ctrl.model_stack <- function(x, verbose = FALSE, ...) {
  res <- process_component_models(x, butcher::axe_ctrl)

  res <- exchange(res, "model_defs", list())

  add_butcher_attributes(
    res,
    x,
    disabled = c("print()", "summary()"),
    add_class = TRUE,
    verbose = verbose
  )
}

#' @importFrom butcher axe_data
#' @export
butcher::axe_data

#' Remove the training data.
#'
#' @rdname axe_model_stack
#' @importFrom butcher axe_data
#' @method axe_data model_stack
#' @export axe_data.model_stack
#' @export
axe_data.model_stack <- function(x, verbose = FALSE, ...) {
  res <- process_component_models(x, butcher::axe_data)

  res <- exchange(res, "train", tibble::tibble())
  res <- exchange(res, "splits", list())
  res <- exchange(res, "data_stack", tibble::tibble())

  add_butcher_attributes(
    res,
    x,
    disabled = c("print()", "summary()"),
    add_class = TRUE,
    verbose = verbose
  )
}

#' @importFrom butcher axe_env
#' @export
butcher::axe_env

#' Remove environments.
#'
#' @rdname axe_model_stack
#' @importFrom butcher axe_env
#' @method axe_env model_stack
#' @export axe_env.model_stack
#' @export
axe_env.model_stack <- function(x, verbose = FALSE, ...) {
  res <- process_component_models(x, butcher::axe_env)

  add_butcher_attributes(
    res,
    x,
    disabled = c("print()", "summary()"),
    add_class = TRUE,
    verbose = verbose
  )
}

#' @importFrom butcher axe_fitted
#' @export
butcher::axe_fitted

#' Remove fitted values.
#'
#' @rdname axe_model_stack
#' @importFrom butcher axe_fitted
#' @method  axe_fitted model_stack
#' @export axe_fitted.model_stack
#' @export
axe_fitted.model_stack <- function(x, verbose = FALSE, ...) {
  res <- process_component_models(x, butcher::axe_fitted)

  add_butcher_attributes(
    res,
    x,
    disabled = c("print()", "summary()"),
    add_class = TRUE,
    verbose = verbose
  )
}


# --------------------------------------------------------------------
# helpers
process_component_models <- function(model_stack, fxn) {
  res <- model_stack

  res[["coefs"]] <- fxn(res[["coefs"]])

  res[["member_fits"]] <-
    purrr::map(
      res[["member_fits"]],
      process_member_fit,
      fxn
    )

  res
}

process_member_fit <- function(member_fit, fxn) {
  fxn(member_fit)
}

# copied from tidymodels/butcher
exchange <- function(x, component, replacement, addition = NULL, old) {
  out <- purrr::pluck(x, component, .default = NA)

  if (!rlang::is_na(out)[1]) {
    x[[component]] <- replacement
    if (!is.null(addition) & !missing(old)) {
      if (!is.null(old[[component]][[addition]])) {
        x[[component]][[addition]] <- old[[component]][[addition]]
      }
    }
  }

  x
}

# butcher attributes helper
add_butcher_disabled <- function(x, disabled = NULL) {
  current <- attr(x, "butcher_disabled")

  if (!is.null(disabled)) {
    disabled <- union(current, disabled)
    attr(x, "butcher_disabled") <- disabled
  }

  x
}

# class assignment helper
add_butcher_class <- function(x) {
  if (!any(grepl("butcher", class(x)))) {
    class(x) <- append(paste0("butchered_", class(x)[1]), class(x))
  }

  x
}

# butcher attributes wrapper
add_butcher_attributes <- function(
  x,
  old,
  disabled = NULL,
  add_class = TRUE,
  verbose = FALSE
) {
  if (!identical(x, old)) {
    x <- add_butcher_disabled(x, disabled)
    if (add_class) {
      x <- add_butcher_class(x)
    }
  }

  if (verbose & !missing(old)) {
    assess_object(old, x)
  }

  x
}
