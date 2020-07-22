#' @export
print.data_stack <- function(x, ...) {
  mode <- attr(x, "mode")
  
  if (mode == "regression") {
    n_members <- ncol(x) - 1
    n_model_defs <- length(attr(x, "model_defs"))
    outcome_name <- colnames(x)[1]
    submodel_lengths <- purrr::map(attr(x, "cols_map"), length)
    model_names <- names(attr(x, "cols_map"))
  } else if (mode == "classification") {
    n_groups <- length(unique(dplyr::pull(attr(x, "train")[,.get_outcome(x)])))
    n_members <- (ncol(x) - 1) / n_groups
    n_model_defs <- length(attr(x, "model_defs"))
    outcome_name <- colnames(x)[1]
    submodel_lengths <- 
      purrr::map_dbl(attr(x, "cols_map"), length) / n_groups
    model_names <- names(attr(x, "cols_map"))
  } else {
    n_members <- 0
    n_groups <- n_members <- n_model_defs <- submodel_lengths <- 0
    outcome_name <- model_names <- NULL
  }
  
  cat(glue::glue("# A data stack with {n_model_defs} model definition",
                 "{if (n_model_defs != 1) 's' else ''}",
                 " and {n_members} candidate member",
                 "{if (n_members != 1) 's' else ''}",
                 "{if (n_model_defs != 0) ':' else '.'}"))
  if (n_model_defs != 0) {cat("\n")}
  
  n_by_model_defs <-
    purrr::map2(
      submodel_lengths,
      model_names,
      function(submodels, name) {
        cat(glue::glue(
          "#   {name}: ",
          "{submodels} sub-model",
          "{if (submodels != 1) 's' else ''}")
        )
        cat("\n")
      }
    )
  
  cat(glue::glue(
    "# Outcome: {if (.get_outcome(x) == 'init_') {NULL} else {.get_outcome(x)}}\n"
  ))
}

#' @export
print.model_stack <- function(x, n = 10, ...) {
  mode <- x[["mode"]]
  x_ <- x[["data_stack"]]
  
  if (mode == "regression") {
    n_members <- if (ncol(x_) == 0) {0} else {ncol(x_) - 1}
    n_model_defs <- length(x[["model_defs"]])
    outcome_name <- colnames(x_)[1]
    submodel_lengths <- purrr::map(x[["cols_map"]], length)
    model_names <- names(x[["cols_map"]])
  } else {
    n_groups <- length(unique(dplyr::pull(x[["train"]][,.get_outcome(x)])))
    n_members <- if (ncol(x_) == 0) {0} else {
      (ncol(x_) - 1) / n_groups
    }
    n_model_defs <- length(x[["model_defs"]])
    outcome_name <- colnames(x_)[1]
    submodel_lengths <- 
      purrr::map_dbl(x[["cols_map"]], length) / n_groups
    model_names <- names(x[["cols_map"]])
  }
  
  if (!is.null(x[["member_fits"]])) {
    cat(glue::glue("# A fitted model stack with {length(x$member_fits)} member",
                   "{if (length(x$member_fits) != 1) 's:' else ':'}"))
    cat("\n#   ")
    cat(paste0(names(x$member_fits), collapse = ", "))
  } else {
    cat(glue::glue("# An unfitted model stack with ",
                   "{n_members} candidate member",
                   "{if (n_members != 1) 's ' else ' '}",
                   "from {n_model_defs} model definition",
                   "{if (n_model_defs != 1) 's.' else '.'}"))
    cat("\n")
  }
}

top_coefs <- function(x, n = 10) {
  betas <- 
    .get_glmn_coefs(x$coefs$fit) %>% 
    dplyr::filter(estimate != 0 & terms != "(Intercept)")
  n <- min(n, nrow(betas))
  
  sub_models <-
    purrr::map_dfr(x$cols_map, ~ tibble::tibble(terms = .x), .id = "model_name")
  model_types <- 
    purrr::map(x$model_defs, workflows::pull_workflow_spec) %>% 
    purrr::map_dfr(~ tibble::tibble(model_type = class(.x)[1]), .id = "model_name")
  res <- 
    dplyr::left_join(betas, sub_models, by = "terms") %>% 
    dplyr::left_join(model_types, by = "model_name") %>% 
    dplyr::top_n(n, estimate) %>% 
    dplyr::arrange(dplyr::desc(estimate)) 
  
  if (any(names(res) == "class")) {
    res <- dplyr::select(res, model = model_type, weight = estimate, class)
  } else {
    res <- dplyr::select(res, model = model_type, weight = estimate)
  }
res
}

print_top_coefs <- function(x, n = 10, digits = 3) {
  res <- top_coefs(x, n)
  
  msg <- paste("The", n, "highest weighted sub-models were:")
  rlang::inform(msg)
  print(res)
  invisible(NULL)
}


member_summary <- function(x) {
  betas <- 
    .get_glmn_coefs(x$coefs$fit) %>% 
    dplyr::filter(terms != "(Intercept)")
  all_terms <- unique(betas$terms)
  used_betas <- dplyr::filter(betas, estimate != 0)
  used_terms <- nrow(used_betas)
  
  msg <- paste0("Out of ", length(all_terms), " possible sub-models, the ",
                "ensemble contains ", used_terms, " non-zero ", 
                ifelse(used_terms > 1, "members", "member"), ".")
  rlang::inform(msg)
  if (any(names(betas) == "class")) {
    n_classes <- length(unique(betas$class))
    beta_per_class <- 
      used_betas %>% 
      dplyr::group_by(class) %>% 
      dplyr::count() %>% 
      dplyr::ungroup() %>% 
      dplyr::pull(n) %>% 
      mean() %>% 
      round(2) 
    msg <- paste0("Across the ", n_classes, " classes, the average number of ",
                 "members per class was ", beta_per_class, ".")
    rlang::inform(msg)
  }
  invisible(NULL)
}
 


