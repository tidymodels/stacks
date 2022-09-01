#' Determine stacking coefficients from a data stack
#'
#' @description 
#' Evaluates a data stack by fitting a regularized model on the 
#' assessment predictions from each candidate member to predict 
#' the true outcome.
#' 
#' This process determines the "stacking coefficients" of the model 
#' stack. The stacking coefficients are used to weight the
#' predictions from each candidate (represented by a unique column
#' in the data stack), and are given by the betas of a LASSO model
#' fitting the true outcome with the predictions given in the
#' remaining columns of the data stack.
#' 
#' Candidates with non-zero stacking coefficients are model stack 
#' members, and need to be trained on the full training set (rather
#' than just the assessment set) with [fit_members()]. This function
#' is typically used after a number of calls to [add_candidates()].
#' 
#' @details 
#' Note that a regularized linear model is one of many possible
#' learning algorithms that could be used to fit a stacked ensemble
#' model. For implementations of additional ensemble learning algorithms, see
#' [h2o::h2o.stackedEnsemble()] and [SuperLearner::SuperLearner()].
#' 
#' @param data_stack A `data_stack` object
#' @param penalty A numeric vector of proposed values for total amount of
#'   regularization used in member weighting. Higher penalties will generally 
#'   result in fewer members being included in the resulting model stack, and 
#'   vice versa. The package will tune over a grid formed from the cross 
#'   product of the `penalty` and `mixture` arguments.
#' @param mixture A number between zero and one (inclusive) giving the 
#'   proportion of L1 regularization (i.e. lasso) in the model. `mixture = 1`
#'   indicates a pure lasso model, `mixture = 0` indicates ridge regression, and
#'   values in `(0, 1)` indicate an elastic net. The package will tune over 
#'   a grid formed from the cross product of the `penalty` and `mixture` 
#'   arguments.
#' @param non_negative A logical giving whether to restrict stacking 
#'   coefficients to non-negative values. If `TRUE` (default), 0 is passed as 
#'   the `lower.limits` argument to [glmnet::glmnet()] in fitting the
#'   model on the data stack. Otherwise, `-Inf`.
#' @param metric A call to [yardstick::metric_set()]. The metric(s) to use in 
#'   tuning the lasso penalty on the stacking coefficients. Default values are
#'   determined by [tune::tune_grid()] from the outcome class.
#' @param control An object inheriting from `control_grid` to be passed to
#'   the model determining stacking coefficients. See [tune::control_grid()]
#'   documentation for details on possible values. Note that any `extract`
#'   entry will be overwritten internally.
#' @param times Number of bootstrap samples tuned over by the model that
#'   determines stacking coefficients. See [rsample::bootstraps()] to
#'   learn more.
#' @inheritParams add_candidates
#' 
#' @return A `model_stack` object—while `model_stack`s largely contain the
#' same elements as `data_stack`s, the primary data objects shift from the
#' assessment set predictions to the member models.
#' 
#' @template note_example_data
#' 
#' @examplesIf rlang::is_installed("kernlab")
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
#' # include fewer models by proposing higher penalties
#' reg_st %>% 
#'   blend_predictions(penalty = c(.5, 1))
#' 
#' # allow for negative stacking coefficients 
#' # with the non_negative argument
#' reg_st %>% 
#'   blend_predictions(non_negative = FALSE)
#'   
#' # use a custom metric in tuning the lasso penalty
#' library(yardstick)
#' reg_st %>% 
#'   blend_predictions(metric = metric_set(rmse))
#'   
#' # pass control options for stack blending
#' reg_st %>% 
#'   blend_predictions(
#'     control = tune::control_grid(allow_par = TRUE)
#'   )
#'  
#' # to speed up the stacking process for preliminary
#' # results, bump down the `times` argument:
#' reg_st %>% 
#'   blend_predictions(times = 5)
#'   
#' # the process looks the same with 
#' # multinomial classification models
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
blend_predictions <- function(data_stack, 
                              penalty = 10 ^ (-6:-1),
                              mixture = 1,
                              non_negative = TRUE,
                              metric = NULL,
                              control = tune::control_grid(), 
                              times = 25,
                              meta_learner = NULL,
                              ...) {
  # argument checking ----------------------------------------------------------
  check_inherits(data_stack, "data_stack")
  check_blend_data_stack(data_stack)
  if (!is.null(metric)) {
    check_inherits(metric, "metric_set")
  }
  check_inherits(control, "control_grid")
  check_inherits(times, "numeric")
  
  preds_formula <-
    rlang::new_formula(
      as.name(attr(data_stack, "outcome")), 
      as.name("."), 
      env = rlang::base_env()
    )

  dat <- process_data_stack(data_stack)
  
  # defining the meta-learner spec ---------------------------------------------
  if (is.null(meta_learner)) {
    meta_learner <-
      process_meta_learner(
        data_stack = data_stack, 
        penalty = penalty, 
        mixture = mixture,
        non_negative = non_negative
      )
  }
  
  preds_wf <-
    workflows::workflow() %>%
    workflows::add_recipe(recipes::recipe(preds_formula, data = dat)) %>%
    workflows::add_model(meta_learner)
  
  # processing tuning arguments and tuning -------------------------------------
  if (is.null(meta_learner)) {
    grid <- purrr::cross_df(list(penalty = penalty, mixture = mixture))
  } else {
    grid <- 10
  }
  
  get_models <- function(x) {
    x %>% 
      workflows::extract_fit_parsnip() %>% 
      purrr::pluck("fit")
  }
  
  control$extract <- get_models
  
  candidates <- 
    preds_wf %>%
    tune::tune_grid(
      resamples = rsample::bootstraps(dat, times = times),
      grid = grid,
      metrics = metric,
      control = control
    )
  
  # finalizing and constructing the model stack --------------------------------
  metric <- tune::.get_tune_metric_names(candidates)[1]
  best_param <- tune::select_best(candidates, metric = metric)
  
  coefs <-
    meta_learner %>%
    tune::finalize_model(best_param) %>%
    parsnip::fit(formula = preds_formula, data = dat)
  
  # TODO: make the penalty object structure general
  if (inherits(coefs, c("_elnet", "_multnet", "_lognet"))) {
    metrics <- glmnet_metrics(candidates)
    penalty <- list(
      penalty = best_param$penalty, 
      mixture = best_param$mixture,
      metric = metric
    )
    primary_class <- "linear_stack"
  } else {
    metrics <- best_param
    penalty <- list()
    primary_class <- "general_stack"
  }

  model_stack <- 
    structure(
      list(model_defs = attr(data_stack, "model_defs"),
           coefs = coefs,
           penalty = penalty,
           metrics = metrics,
           equations = get_expressions(coefs),
           cols_map = attr(data_stack, "cols_map"),
           model_metrics = attr(data_stack, "model_metrics"),
           train = attr(data_stack, "train"),
           mode = attr(data_stack, "mode"),
           outcome = attr(data_stack, "outcome"),
           data_stack = dat,
           splits = attr(data_stack, "splits")),
      class = c(primary_class, "model_stack", "list")
    )
  
  if (model_stack_constr(model_stack)) {model_stack}
}

# blending utilities -----------------------------------------------------------
check_regularization <- function(x, arg) {
  if (!is.numeric(x)) {
    glue_stop(
      "The argument to '{arg}' must be a numeric, but the supplied {arg}'s ",
      "class is `{list(class(x))}`"
    )
  }
  
  if (length(x) == 0) {
    glue_stop("Please supply one or more {arg} values.")
  }
  
  if (arg == "penalty") {
    if (any(x < 0)) {
      glue_stop("Please supply only nonnegative values to the {arg} argument.")
    }
  }
  
  if (arg == "mixture") {
    if (any(x < 0 || x > 1)) {
      glue_stop("Please supply only values in [0, 1] to the {arg} argument.")
    }
  }
}

# ------------------------------------------------------------------------------

glmnet_metrics <- function(x) {
  res <- tune::collect_metrics(x)
  pens <- sort(unique(res$penalty))
  num_mem <- 
    dplyr::select(x, id, .extracts) %>% 
    tidyr::unnest(.extracts) %>% 
    dplyr::group_nest(id, penalty, mixture) %>% 
    # There are redundant model objects over penalty values
    dplyr::mutate(data = purrr::map(data, ~ .x$.extracts[[1]])) %>% 
    dplyr::mutate(
      members = purrr::map(data, ~ num_members(.x, pens))
    ) %>% 
    dplyr::select(mixture, members) %>% 
    tidyr::unnest(cols = members) %>% 
    dplyr::group_by(penalty, mixture) %>% 
    dplyr::summarize(
      .metric = "num_members",
      .estimator = "Poisson",
      mean = mean(members, na.rm = TRUE), 
      n = sum(!is.na(members)),
      std_err = sqrt(mean/n),
      .groups = "drop"
    ) %>% 
    dplyr::full_join(
      res %>% dplyr::select(penalty, mixture, .config) %>% dplyr::distinct(),
      by = c("penalty", "mixture")
    )
  
  dplyr::bind_rows(res, num_mem) %>% 
    dplyr::arrange(.config)
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

process_data_stack <- function(data_stack) {
  dat <- tibble::as_tibble(data_stack) %>% na.omit()
  
  if (nrow(dat) == 0) {
    glue_stop(
      "All rows in the data stack have at least one missing value. ",
      "Please ensure that all candidates supply predictions."
    )
  }
  
  if (nrow(dat) < nrow(data_stack)) {
    glue_message(
      "{nrow(data_stack) - nrow(dat)} of the {nrow(data_stack)} rows in the ", 
      "data stack have missing values, and will be omitted in the blending process."
    )
  }
  
  if (attr(data_stack, "mode") != "regression") {
    # The class probabilities add up to one so we remove the probability columns
    # associated with the first level of the outcome. 
    lvls <- levels(data_stack[[attr(data_stack, "outcome")]])
    col_filter <- paste0(".pred_", lvls[1])
    dat <- dat %>% dplyr::select(-dplyr::starts_with(!!col_filter))
  }

  dat
}

process_meta_learner <- function(data_stack = data_stack, 
                                         penalty = penalty, 
                                         mixture = mixture,
                                         non_negative = non_negative) {
  check_regularization(penalty, "penalty")
  check_regularization(mixture, "mixture")
  check_inherits(non_negative, "logical")
  
  dat <- process_data_stack(data_stack)
  
  lvls <- levels(data_stack[[attr(data_stack, "outcome")]])
  
  ll <- if (non_negative) {0} else {-Inf}
  
  tune_quo <- rlang::new_quosure(tune::tune(), env = rlang::empty_env())
  
  if (attr(data_stack, "mode") == "regression") {
    model_spec <- 
      parsnip::linear_reg(penalty = !!tune_quo, mixture = !!tune_quo) %>%
      parsnip::set_engine("glmnet", lower.limits = !!ll, lambda.min.ratio = 0)
  } else {
    if (length(lvls) == 2) {
      model_spec <-
        parsnip::logistic_reg(penalty = !!tune_quo, mixture = !!tune_quo) %>% 
        parsnip::set_engine("glmnet", lower.limits = !!ll, lambda.min.ratio = 0) %>% 
        parsnip::set_mode("classification")
    } else {
      model_spec <-
        parsnip::multinom_reg(penalty = !!tune_quo, mixture = !!tune_quo) %>% 
        parsnip::set_engine("glmnet", lower.limits = !!ll, lambda.min.ratio = 0) %>% 
        parsnip::set_mode("classification")
    }
  }
    
  model_spec
}





