#' @export
print.data_stack <- function(x, ...) {
  
}

#' @export
print.model_stack <- function(x, n = 10, ...) {
  fitted <- if (is.null(x[["member_fits"]])) {"n unfitted "} else {" fitted "}
  mode <- x[["mode"]]
  is_reg <- mode == "regression"
  
  intro <- paste0("A", fitted, mode, " model stack.")
  rlang::inform(intro)
  
  member_summary(x)
  
  print_top_coefs(x)
  
  invisible(NULL)
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
    res <- dplyr::select(res, member = terms, type = model_type, weight = estimate)
  } else {
    res <- dplyr::select(res, member = terms, type = model_type, weight = estimate)
  }
  
  res
}

print_top_coefs <- function(x, n = 10, digits = 3) {
  res <- top_coefs(x, n)
  
  msg <- paste0("The highest weighted member",
               if (x$mode == "regression") {"s"} else {" classes"},
               " are:")
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
  
  msg <- paste0("Out of ", length(all_terms), " possible candidates, the ",
                "ensemble contains ", used_terms, 
                ifelse(used_terms > 1, " members", " member"), ".")
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
