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
  ".metric",
  ".pred",
  ".pred_class",
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
  "terms",
  "type",
  "value",
  "weight",
  "weighted_est",
  "where"
))

# Checks and Prompts
# ------------------------------------------------------------------------
# wrappers for prompting with glue with appropriate colors
glue_stop <- function(..., .sep = "", .envir = parent.frame()) {
  glue_prompt(
    ..., 
    .sep = .sep, 
    .envir = .envir, 
    type = "danger", 
    rlang_fn = rlang::abort
  )
}

glue_warn <- function(..., .sep = "", .envir = parent.frame()) {
  glue_prompt(
    ..., 
    .sep = .sep, 
    .envir = .envir, 
    type = "warning", 
    rlang_fn = rlang::warn
  )
}

glue_message <- function(..., .sep = "", .envir = parent.frame()) {
  glue_prompt(
    ..., 
    .sep = .sep, 
    .envir = .envir, 
    type = "info", 
    rlang_fn = rlang::inform
  )
}

# takes in a prompt and a prompt type and colors the
# prompt according to the prompt type
color_prompt <- function(prompt, type) {
  colors <- tune::get_tune_colors()
  
  prompt_fn <- colors[["message"]][[type]]
  
  prompt_fn(prompt)
}

# takes in a vector, parses it with glue, wraps to the console width, colors
# it with the appropriate tune color, and raises it with the appropriate prompt
glue_prompt <- function(..., .sep = "", .envir = parent.frame(), type, rlang_fn) {
  glue::glue(..., .sep = .sep, .envir = .envir) %>%
    glue::glue_collapse() %>%
    color_prompt(type) %>%
    rlang_fn()
}

# adapted from tune
check_empty_ellipses <- function(...) {
  dots <- rlang::enquos(...)
  if (length(dots) > 0) {
    needs_name <- names(dots) == ""
    names(dots)[needs_name] <- 
      dots[needs_name] %>%
      purrr::map(
        rlang::get_expr
      ) %>%
      unlist()
    
    msg <- "The `...` are not used in this function but one or more arguments were passed: "
    msg <- paste0(msg, paste0("'", names(dots), "'", collapse = ", "))
    rlang::warn(msg)
  }
  invisible(NULL)
}

check_inherits <- function(x, what) {
  cl <- match.call()
  
  if (!inherits(x, what)) {
    glue_stop("Element `{list(cl$x)}` needs to inherit from `{what}`, but its ",
              "class is `{list(class(x))}`.")
  }
  
  invisible(TRUE)
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

.get_rs_hash <- function(stack) {attr(stack, "rs_hash")}

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
  x <- tibble::as_tibble(x) %>% dplyr::mutate(terms = rn, penalty = penalty)
  x <- dplyr::select(x, terms, estimate, penalty)
  if (is.list(x$estimate)) {
    x$estimate <- purrr::map(x$estimate, ~ tibble::as_tibble(as.matrix(.x), rownames = "terms"))
    x <- tidyr::unnest(x, cols = c(estimate), names_repair = "minimal")
    names(x) <- c("class", "terms", "estimate", "penalty")
  }
  x
}

# quiet R-CMD-check NOTEs that workflowsets and yardstick are unused
# (see example data .Rmds for usage)
#' @importFrom yardstick metric_set
#' @importFrom workflowsets workflow_set
NULL
