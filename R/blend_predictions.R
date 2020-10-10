#' Evaluate a data stack
#'
#' Evaluates a stack by performing regularization on the out-of-sample
#' predictions to determine coefficients for the combining of predictions
#' from ensemble members.
#' 
#' @param data_stack A `data_stack` object
#' @param penalty A numeric vector of proposed penalty values used in member
#'   weighting. Higher penalties will generally result in fewer members 
#'   being included in the resulting model stack, and vice versa. This argument
#'   will be tuned on unless a single penalty value is given.
#' @param verbose A logical for logging results as they are generated. Despite 
#'   this argument, warnings and errors are always shown.
#' @inheritParams add_candidates
#' 
#' @return A `model_stack` object—while `model_stacks` largely contain the
#' same elements as `data_stack`s, the primary data objects shift from the
#' assessment set predictions to the member models.
#' 
#' @template note_example_data
#' 
#' @examples 
#' \donttest{
#' # see the "Example Data" section above for
#' # clarification on the objects used in these examples!
#' 
#' # put together a data stack
#' reg_st <- 
#'   stacks() %>%
#'   add_candidates(reg_res_lr) %>%
#'   add_candidates(reg_res_svm) %>%
#'   add_candidates(reg_res_sp)
#'   
#' reg_st
#'
#' # evaluate the data stack
#' reg_st %>%
#'   blend_predictions()
#' 
#' # include fewer models by proposing
#' # higher penalties
#' reg_st %>% blend_predictions(penalty = c(.5, 1))
#'   
#' # do the same with multinomial classification models
#' class_st <-
#'   stacks() %>%
#'   add_candidates(class_res_nn) %>%
#'   add_candidates(class_res_rf) %>%
#'   blend_predictions()
#'   
#' class_st
#' 
#' # ...or binomial classification models
#' log_st <-
#'   stacks() %>%
#'   add_candidates(log_res_nn) %>%
#'   add_candidates(log_res_rf) %>%
#'   blend_predictions()
#'   
#' log_st
#' }
#' 
#' @family core verbs
#' @export
blend_predictions <- function(data_stack, penalty = 10 ^ (-6:-1), verbose = FALSE, ...) {
  check_inherits(data_stack, "data_stack")
  check_blend_data_stack(data_stack)
  check_penalty(penalty)
  check_inherits(verbose, "logical")
  
  outcome <- attr(data_stack, "outcome")
  
  preds_formula <- 
    paste0(outcome, " ~ .") %>%
    as.formula()
  lvls <- levels(data_stack[[outcome]])
  
  dat <- tibble::as_tibble(data_stack)
  
  if (attr(data_stack, "mode") == "regression") {
    model_spec <- 
      parsnip::linear_reg(penalty = tune::tune(), mixture = 1) %>%
      parsnip::set_engine("glmnet", lower.limits = 0)
    
    metric <- yardstick::metric_set(yardstick::rmse)
    
    preds_wf <-
      workflows::workflow() %>%
      workflows::add_model(model_spec) %>%
      workflows::add_formula(preds_formula)
  } else {
    # The class probabilities add up to one so we remove the probability columns
    # associated with the first level of the outcome. 
    col_filter <- paste0(".pred_", lvls[1])
    dat <- dat %>% dplyr::select(-dplyr::starts_with(!!col_filter))
    if (length(lvls) == 2) {
      model_spec <-
        parsnip::logistic_reg(penalty = tune::tune(), mixture = 1) %>% 
        parsnip::set_engine("glmnet", lower.limits = 0) %>% 
        parsnip::set_mode("classification")
    } else {
      model_spec <-
        parsnip::multinom_reg(penalty = tune::tune(), mixture = 1) %>% 
        parsnip::set_engine("glmnet", lower.limits = 0) %>% 
        parsnip::set_mode("classification")
    }
    metric <- yardstick::metric_set(yardstick::roc_auc)
    
    preds_wf <- 
      workflows::workflow() %>%
      workflows::add_recipe(
        recipes::recipe(
          preds_formula, 
          data = dat
          )
      ) %>%
      workflows::add_model(model_spec)
  }
  
  get_models <- function(x) {
    x %>% 
      workflows::pull_workflow_fit() %>% 
      purrr::pluck("fit")
  }
  
  splits <- attr(data_stack, "splits")
  if (inherits(splits[[1]], "val_split")) {
    rs <-  rsample::bootstraps(dat, times = 20)
  } else {
    rs <- reconstruct_resamples(attr(data_stack, "splits"), dat)
  }
  
  candidates <- 
    preds_wf %>%
    tune::tune_grid(
      resamples = rs,
      grid = tibble::tibble(penalty = penalty),
      metrics = metric,
      control = tune::control_grid(save_pred = TRUE, extract = get_models)
    )
  
  metric <- tune::.get_tune_metric_names(candidates)[1]
  best_param <- tune::select_best(candidates, metric = metric)
  coefs <-
    model_spec %>%
    tune::finalize_model(best_param) %>%
    generics::fit(formula = preds_formula, data = dat)
  
  
  
  model_stack <- 
    structure(
      list(model_defs = attr(data_stack, "model_defs"),
           coefs = coefs,
           penalty = list(penalty = best_param$penalty, metric = metric),
           metrics = glmnet_metrics(candidates),
           equations = get_expressions(coefs),
           cols_map = attr(data_stack, "cols_map"),
           model_metrics = attr(data_stack, "model_metrics"),
           train = attr(data_stack, "train"),
           mode = attr(data_stack, "mode"),
           outcome = attr(data_stack, "outcome"),
           data_stack = dat,
           splits = attr(data_stack, "splits")),
      class = c("linear_stack", "model_stack", "list")
    )
  
  if (model_stack_constr(model_stack)) {model_stack}
}

# makes an rsample-like object out of its splits
# and replaces the training data with the stack data
reconstruct_resamples <- function(splits, data) {
  res <- splits
  
  res[["splits"]] <-
    purrr::map(
      res[["splits"]],
      function(x) {x[["data"]] <- data; x}
    )
  
  new_classes <- c(attr(splits, "rset_info")[["att"]][["class"]], 
                   "rset", 
                   class(res))
  
  
  res <- safe_attr(res, attr(splits, "rset_info")[["att"]])
  
  structure(res, class = new_classes)
}

check_penalty <- function(x) {
  if (!is.numeric(x)) {
    glue_stop(
      "The argument to 'penalty' must be a numeric, but the supplied penalty's ",
      "class is `{list(class(x))}`"
    )
  }
  
  if (length(x) == 0) {
    glue_stop("Please supply one or more penalty values.")
  }
  
  if (any(x < 0)) {
    glue_stop("Please supply only nonnegative values to the penalty argument.")
  }
}

# ------------------------------------------------------------------------------

glmnet_metrics <- function(x) {
  res <- tune::collect_metrics(x)
  pens <- sort(unique(res$penalty))
  x$glmnet_fits <- purrr::map(x$.extracts, ~ .x$.extracts[[1]])
  num_mem <- 
    purrr::map_dfr(x$glmnet_fits, num_members, pens) %>% 
    dplyr::group_by(penalty) %>% 
    dplyr::summarize(
      .metric = "num_members",
      .estimator = "Poisson",
      mean = mean(members, na.rm = TRUE), 
      n = sum(!is.na(members)),
      std_err = sqrt(mean/n)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::full_join(
      res %>% dplyr::select(penalty, .config) %>% dplyr::distinct(),
      by = "penalty"
    )
  dplyr::bind_rows(res, num_mem)
}

num_members <- function(x, penalties) {
  glmn_coef <-  coef(x, s = penalties)
  if (is.list(glmn_coef)) {
    glmn_coef <- do.call("rbind", glmn_coef)
  }
  glmn_coef <- glmn_coef[rownames(glmn_coef) != "(Intercept)",,drop = FALSE]
  mems <- apply(glmn_coef, 2, function(x) sum(x != 0))
  tibble::tibble(penalty = penalties, members = unname(mems))  
}

# set attributes from new_attr that are not
# already set in x
safe_attr <- function(x, new_attr) {
  res <- x
  
  x_attr <- attributes(x)
  
  dup_attrs <- names(new_attr) %in% names(x_attr)
  
  attributes(res) <- c(x_attr, new_attr[!dup_attrs])
  
  attr(res, "rset_info") <- NULL
  
  res
}

check_blend_data_stack <- function(data_stack) {
  # many possible checks we could do here are redundant with those we
  # carry out in fit_members() -- just check for bare stacks, 1-candidate
  # stacks, and non-stack objects
  if (!inherits(data_stack, "data_stack")) {
    check_inherits(data_stack, "data_stack")
  } else if (ncol(data_stack) == 0) {
      glue_stop(
        "The data stack supplied as the argument to `data_stack` has no ",
        "candidate members. Please first add candidates with ",
        "the `add_candidates()` function."
      )
  } else if ((ncol(data_stack) == 2 && attr(data_stack, "mode") == "regression") || 
             ncol(data_stack) == length(levels(data_stack[[1]])) + 1) {
    glue_stop(
      "The supplied data stack only contains one candidate member. Please ",
      "add more candidate members using `add_candidates()` before blending."
    )
  }
  
  invisible(NULL)
}

