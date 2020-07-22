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
#' @inheritParams stack_add
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
#'   stack_add(reg_res_lr) %>%
#'   stack_add(reg_res_svm) %>%
#'   stack_add(reg_res_sp)
#'   
#' reg_st
#'
#' # evaluate the data stack
#' reg_st %>%
#'   stack_blend()
#' 
#' # include fewer models by proposing
#' # higher penalties
#' reg_st %>% stack_blend(penalty = c(.5, 1))
#'   
#' # do the same with multinomial classification models
#' class_st <-
#'   stacks() %>%
#'   stack_add(class_res_nn) %>%
#'   stack_add(class_res_rf) %>%
#'   stack_blend()
#'   
#' class_st
#' 
#' # ...or binomial classification models
#' log_st <-
#'   stacks() %>%
#'   stack_add(log_res_nn) %>%
#'   stack_add(log_res_rf) %>%
#'   stack_blend()
#'   
#' log_st
#' }
#' 
#' @family core verbs
#' @export
stack_blend <- function(data_stack, penalty = 10 ^ (-6:-1), verbose = FALSE, ...) {
  outcome <- attr(data_stack, "outcome")
  
  check_penalty(penalty)
  
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
  
  candidates <- 
    preds_wf %>%
    tune::tune_grid(
      resamples = reconstruct_resamples(attr(data_stack, "splits"), dat),
      grid = tibble::tibble(penalty = penalty),
      metrics = metric,
      control = tune::control_grid(save_pred = TRUE, extract = get_models)
    )
  
  coefs <-
    model_spec %>%
    tune::finalize_model(tune::select_best(candidates)) %>%
    generics::fit(formula = preds_formula, data = dat)
  
  model_stack <- 
    structure(
      list(model_defs = attr(data_stack, "model_defs"),
           coefs = coefs,
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
  res <- 
    splits %>%
    tibble::enframe(name = NULL, value = "splits") %>%
    dplyr::mutate(
      id = purrr::map_chr(splits, function(x) dplyr::pull(x[["id"]]))
    )
    
  res[["splits"]] <-
    purrr::map(
      res[["splits"]],
      function(x) {x[["data"]] <- data; x}
    )
  
  structure(res, class = c("rset", class(res)))
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
  
  if (any(x <= 0)) {
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
      mean = mean(members), 
      n = sum(!is.na(members)),
      std_err = sqrt(mean(members)/sum(!is.na(members)))
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::full_join(
      res %>% dplyr::select(penalty, .config) %>% dplyr::distinct(),
      by = "penalty"
    )
  dplyr::bind_rows(res, num_mem)
}

num_members <- function(x, penalties) {
  mems <- 
    coef(x, s = penalties) %>% 
    apply(2, function(x) sum(x != 0))
  tibble::tibble(penalty = penalties, members = unname(mems))  
}

