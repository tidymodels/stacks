#' Control wrappers
#'
#' @description
#' Supply these light wrappers as the `control` argument in a
#' [tune::tune_grid()], [tune::tune_bayes()], or [tune::fit_resamples()]
#' call to return the needed elements for use in a data stack.
#' These functions will return the appropriate control grid to ensure that
#' assessment set predictions and information on model specifications and
#' preprocessors, is supplied in the resampling results object!
#'
#' To integrate stack settings with your existing control settings, note
#' that these functions just call the appropriate `tune::control_*` function
#' with the arguments `save_pred = TRUE, save_workflow = TRUE`.
#'
#' @return A [tune::control_grid], [tune::control_bayes],
#' or [tune::control_resamples] object.
#'
#' @seealso See [example_data] for examples of these functions used in context.
#'
#' @examples
#' library(tune)
#'
#' # these are the same!
#' control_stack_grid()
#' control_grid(save_pred = TRUE, save_workflow = TRUE)
#'
#' @name control_stack
NULL

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
