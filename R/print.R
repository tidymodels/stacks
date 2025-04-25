#' @export
print.data_stack <- function(x, ...) {
  mode <- attr(x, "mode")

  if (mode == "regression") {
    n_cands <- ncol(x) - 1
    n_model_defs <- length(attr(x, "model_defs"))
    outcome_name <- colnames(x)[1]
    submodel_lengths <- purrr::map(attr(x, "cols_map"), length)
    model_names <- names(attr(x, "cols_map"))
  } else if (mode == "classification") {
    n_groups <- length(unique(dplyr::pull(attr(x, "train")[, .get_outcome(x)])))
    n_cands <- (ncol(x) - 1) / n_groups
    n_model_defs <- length(attr(x, "model_defs"))
    outcome_name <- colnames(x)[1]
    submodel_lengths <-
      purrr::map_dbl(attr(x, "cols_map"), length) / n_groups
    model_names <- names(attr(x, "cols_map"))
  } else {
    n_cands <- 0
    n_groups <- n_cands <- n_model_defs <- submodel_lengths <- 0
    outcome_name <- model_names <- NULL
  }

  cat(glue::glue(
    "# A data stack with {n_model_defs} model definition",
    "{if (n_model_defs != 1) 's' else ''}",
    " and {n_cands} candidate member",
    "{if (n_cands != 1) 's' else ''}",
    "{if (n_model_defs != 0) ':' else '.'}"
  ))
  if (n_model_defs != 0) {
    cat("\n")
  }

  n_by_model_defs <-
    purrr::map2(
      submodel_lengths,
      model_names,
      function(submodels, name) {
        cat(glue::glue(
          "#   {name}: ",
          "{submodels} model configuration",
          "{if (submodels != 1) 's' else ''}"
        ))
        cat("\n")
      }
    )

  cat(glue::glue(
    "# Outcome: {if (.get_outcome(x) == 'init_') {NULL} else {.get_outcome(x)}}",
    paste0(
      if (.get_outcome(x) != 'init_') {
        c(" (", class(dplyr::pull(x[, 1])), ")")
      } else {
        NULL
      },
      collapse = ""
    )
  ))
}

#' @export
print.model_stack <- function(x, n = 10, ...) {
  cli::cli_text(cli::rule(
    "A stacked ensemble model",
    width = min(65, cli::console_width())
  ))

  member_summary(x)

  print_top_coefs(x)

  if (is.null(x[["member_fits"]])) {
    cli::cli_text("\n")
    cli::cli_text("Members have not yet been fitted with `fit_members()`.")
  }

  invisible(NULL)
}

#' @export
print.butchered_linear_stack <- function(x, ...) {
  cli::cli_text(cli::rule(
    "A stacked ensemble model",
    width = min(65, cli::console_width())
  ))

  cli::cli_text("\n")
  cli::cli_text("Print methods for butchered model stacks are disabled.")
}

top_coefs <- function(x, penalty = x$penalty$penalty, n = 10) {
  betas <-
    .get_glmn_coefs(x$coefs$fit, penalty = penalty) |>
    dplyr::filter(estimate != 0 & terms != "(Intercept)")
  n <- min(n, nrow(betas))

  sub_models <-
    purrr::map(x$cols_map, function(.x) tibble::tibble(terms = .x)) |>
    purrr::list_rbind(names_to = "model_name")
  model_types <-
    purrr::map(x$model_defs, workflows::extract_spec_parsnip) |>
    purrr::map(function(.x) tibble::tibble(model_type = class(.x)[1])) |>
    purrr::list_rbind(names_to = "model_name")
  res <-
    dplyr::left_join(betas, sub_models, by = "terms") |>
    dplyr::left_join(model_types, by = "model_name") |>
    dplyr::top_n(n, abs(estimate)) |>
    dplyr::arrange(dplyr::desc(abs(estimate)))

  if (any(names(res) == "class")) {
    pred_levels <-
      x$train |>
      dplyr::select(!!.get_outcome(x)) |>
      dplyr::pull() |>
      levels()

    pred_strings <- paste0(".pred_", pred_levels, "_")

    res <-
      res |>
      # possible code to split the pred class and (actual) member
      # dplyr::mutate(
      #   member_ = gsub(
      #     pattern = paste0(pred_strings, collapse = "|"),
      #     x = terms,
      #     replacement = ""
      #   ),
      #   class = gsub(
      #     pattern = paste0(paste0("_", member_), collapse = "|"),
      #     x = terms,
      #     replacement = ""
      #   ),
      #   class = gsub(".pred_", x = class, rep = "")
      # ) |>
      dplyr::mutate(class = factor(class, levels = pred_levels)) |>
      dplyr::select(member = terms, type = model_type, weight = estimate, class)
  } else {
    res <- dplyr::select(
      res,
      member = terms,
      type = model_type,
      weight = estimate
    )
  }

  res
}

print_top_coefs <- function(
  x,
  penalty = x$penalty$penalty,
  n = 10,
  digits = 3
) {
  res <- top_coefs(x, penalty = penalty, n = n)

  cli::cli_text("\n")
  values <- if (x$mode == "regression") {
    "s"
  } else {
    " classes"
  }
  cli::cli_text("The {nrow(res)} highest weighted member{values} are:")
  print(res)
  invisible(NULL)
}

member_summary <- function(x, penalty = x$penalty$penalty) {
  betas <-
    .get_glmn_coefs(x$coefs$fit, penalty = penalty) |>
    dplyr::filter(terms != "(Intercept)")
  all_terms <- length(unique(betas$terms))
  used_betas <- dplyr::filter(betas, estimate != 0)
  used_terms <- nrow(used_betas)

  msg <- c(
    "",
    "Out of {all_terms} possible candidate members, the ensemble \\
     retained {used_terms}.",
    "Penalty: {.val {x$penalty$penalty}}.",
    "Mixture: {.val {x$penalty$mixture}}."
  )

  cli::cli_bullets(msg)
  if (any(names(betas) == "class")) {
    n_classes <- length(unique(betas$class))
    beta_per_class <-
      used_betas |>
      dplyr::group_by(class) |>
      dplyr::count() |>
      dplyr::ungroup() |>
      dplyr::pull(n) |>
      mean() |>
      round(2)
    msg <- "Across the {n_classes} classes, there are an average \\
            of {beta_per_class} coefficients per class."
    cli::cli_bullets(msg)
  }
  invisible(NULL)
}
