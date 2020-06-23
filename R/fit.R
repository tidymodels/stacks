#' @importFrom generics fit
#' @export
generics::fit

#' Fit an evaluated model stack.
#' 
#' @param object Stacked data.
# @param n The total number of sub-models to incorporate in the stack.
# @param replace Logical—whether to sample the sub-models to incorporate
# in the stack with replacement.
# @param n_initial The number of best sub-models to initialize the stack with
# before initiating subset selection.
# @param bag_p Numeric in (0, 1]—the proportion of models in the bag at
# each iteration.
#' @inheritParams model_stack
#' 
#' @rdname fit
#' @export
fit.stacked_data <- function(object, ...) {
  stacked_preds <- object$stacked_data
  
  preds_formula <- 
    paste0(colnames(stacked_preds)[1], " ~ .") %>%
    as.formula()
  
  stack_coefs <- 
    parsnip::linear_reg(penalty = tune::tune(), mixture = 1) %>%
    parsnip::set_engine("glmnet", lower.limits = 0) %>%
    tune::tune_grid(
      preds_formula,
      resamples = rsample::bootstraps(stacked_preds),
      grid = tibble::tibble(penalty = 10 ^ (-6:-1)),
      metrics = yardstick::metric_set(yardstick::rmse)
    )
  
  structure(
      list(
        coefs = stack_coefs
      ),
      class = c("stack_fit", "list")
  )
}
