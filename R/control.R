#' Control wrappers
#'
#' Supply these light wrappers as the `control` argument in a
#' [tune::tune_grid], [tune::tune_bayes], or [tune::fit_resamples]
#' call to return the needed elements for use in a data stack.
#' These functions will return the appropriate control grid to ensure that
#' assessment set predictions and information on model specifications and
#' preprocessors, is supplied in the resampling results object!
#' 
#' @return A [tune::control_grid], [tune::control_bayes], 
#' or [tune::control_resamples] object.
#' 
#' @seealso See [example_data] for examples of these functions used in context.
#'
#' @rdname control_stack
#' @export
control_stack_grid <- function() {
  tune::control_grid(
    save_pred = TRUE,
    save_workflow = TRUE
  )
}

#' @rdname control_stack
#' @export
control_stack_resamples <- function() {
  tune::control_resamples(
    save_pred = TRUE,
    save_workflow = TRUE
  )
}

#' @rdname control_stack
#' @export
control_stack_bayes <- function() {
  tune::control_bayes(
    save_pred = TRUE,
    save_workflow = TRUE
  )
}