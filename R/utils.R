# Re-exports
# ------------------------------------------------------------------------
#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

# Imports
#' @importFrom cli cli_inform
#' @importFrom cli cli_warn
#' @importFrom cli cli_abort
#' @importFrom cli qty
#' @importFrom rlang caller_env %||%
#' @import workflows

# Global Variables
# ------------------------------------------------------------------------
utils::globalVariables(c(
  ":=",
  ".",
  ".config",
  ".metric",
  ".pred",
  ".pred_class",
  ".sum",
  "across",
  "any_of",
  "as.formula",
  "assess_object",
  "coef",
  "contains",
  "data",
  "estimate",
  "estimate.x",
  "estimate.y",
  ".extracts",
  "id",
  "idx",
  "lp",
  "mem",
  "member",
  "members",
  "mixture",
  "model",
  "model_type",
  "n",
  "name",
  "name.x",
  "na.omit",
  "new",
  "penalty",
  "pred_class",
  "pred_class_sum",
  "pred_class_sum_norm",
  "rowid",
  "setNames",
  "splits",
  "stack",
  "starts_with",
  "terms",
  "type",
  "value",
  "weight",
  "weighted_est",
  "where"
))

# Checks and Prompts
# ------------------------------------------------------------------------
# adapted from tune
check_empty_ellipses <- function(...) {
  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    needs_name <- names(dots) == ""
    names(dots)[needs_name] <-
      dots[needs_name] |>
      purrr::map(
        rlang::get_expr
      ) |>
      unlist()

    cli_warn(
      "The `...` are not used in this function but {?an/}
       argument{?s} {.arg {names(dots)}} {?was/were} passed."
    )
  }
  invisible(NULL)
}

check_inherits <- function(x, what, call = caller_env()) {
  cl <- match.call()

  if (!inherits(x, what)) {
    cli_abort(
      "Element {.val {cl$x}} needs to inherit from {.var {what}}, but its 
       class is {.var {class(x)}}.",
      call = call
    )
  }

  invisible(TRUE)
}

# adapted from ps:::is_cran_check()
is_cran_check <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    FALSE
  } else {
    Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
  }
}

# suggests: a character vector of package names, giving packages
#           listed in Suggests that are needed for the example.
# for use a la `@examplesIf (tune:::should_run_examples())`
should_run_examples <- function(suggests = NULL) {
  has_needed_installs <- TRUE

  if (!is.null(suggests)) {
    has_needed_installs <- rlang::is_installed(suggests)
  }

  has_needed_installs && !is_cran_check()
}

mode_is_regression <- function(x) {
  isTRUE(x[["mode"]] %in% "regression")
}


# Getters
# -----------------------------------------------------------------------
.get_outcome <- function(stack) {
  if (!is.null(attr(stack, "outcome"))) {
    attr(stack, "outcome")
  } else {
    stack[["outcome"]]
  }
}

.get_rs_hash <- function(stack) {
  attr(stack, "rs_hash")
}

.get_model_def_names <- function(stack) {
  if (!is.null(names(attr(stack, "model_defs")))) {
    names(attr(stack, "model_defs"))
  } else {
    names(stack[["model_defs"]])
  }
}

# get the coefficients from the best glmnet result
.get_glmn_coefs <- function(x, penalty = 0.01) {
  x <- glmnet::coef.glmnet(x, s = penalty)
  x <- as.matrix(x)
  colnames(x) <- "estimate"
  rn <- rownames(x)
  x <- tibble::as_tibble(x)
  x$terms <- rn
  x$penalty <- penalty
  x <- x[, c("terms", "estimate", "penalty")]

  if (is.list(x$estimate)) {
    x$estimate <- purrr::map(
      x$estimate,
      function(.x) tibble::as_tibble(as.matrix(.x), rownames = "terms")
    )
    x <- tidyr::unnest(x, cols = c(estimate), names_repair = "minimal")
    names(x) <- c("class", "terms", "estimate", "penalty")
  }
  x
}
