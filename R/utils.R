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
  "estimate",
  "estimate.x",
  "estimate.y",
  "id",
  "idx",
  "lp",
  "mem",
  "member",
  "members",
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
  "weighted_est"
))

# Checks and Prompts
# ------------------------------------------------------------------------
# wrappers for prompting with glue with appropriate colors
glue_stop <- function(..., .sep = "", .envir = parent.frame()) {
  glue::glue(..., .sep = .sep, .envir = .envir) %>%
    color_prompt("danger") %>%
    rlang::abort()
}

glue_warn <- function(..., .sep = "", .envir = parent.frame()) {
  glue::glue(..., .sep = .sep, .envir = .envir) %>%
    color_prompt("warning") %>%
    rlang::warn()
}

glue_message <- function(..., .sep = "", .envir = parent.frame()) {
  glue::glue(..., .sep = .sep, .envir = .envir) %>%
    color_prompt("info") %>%
    rlang::inform()
}

# takes in a prompt and a prompt type and colors the
# prompt according to the prompt type
color_prompt <- function(prompt, type) {
  colors <- tune::get_tune_colors()
  
  prompt_fn <- colors[["message"]][[type]]
  
  prompt_fn(prompt)
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
