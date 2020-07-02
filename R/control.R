#' Compile a minimal workflow for use in stacks
#'
#' Supply this helper function as the `extract` argument in a
#' [tune::control_grid], [tune::control_bayes], or [tune::control_resamples]
#' call to return the needed workflow elements for use in a data stack.
#' 
#' @param x A `workflow` object.
#' 
#' @return A minimal `workflow` object with only the model specification and
#' preprocessor.
#' 
#' @seealso See [control_stack_grid], [control_stack_bayes],
#' and [control_stack_resamples] for wrappers around this function that
#' can be supplied directly to [tune::control_grid], [tune::control_bayes], 
#' or [tune::control_resamples], respectively
#' 
#' @examples 
#' \dontrun{
#' library(tune)
#' 
#' tune::control_grid(save_pred = TRUE,
#'                    extract = stack_workflow)
#' }
#' 
#' @export
stack_workflow <- function(x) {
  workflows::workflow() %>%
    recipes::add_model(workflows::pull_workflow_spec(x)) %>%
    recipes::add_recipe(workflows::pull_workflow_preprocessor(x))
}

#' Control wrappers
#'
#' Supply these light wrappers as the `control` argument in a
#' [tune::tune_grid], [tune::tune_bayes], or [tune::fit_resamples]
#' call to return the needed workflow elements for use in a data stack.
#' These functions will return the appropriate control grid to ensure that
#' a minimal workflow giving details on model specifications and
#' preprocessors is supplied in the resampling results object!
#' 
#' @return A [tune::control_grid], [tune::control_bayes], 
#' or [tune::control_resamples] object.
#' 
#' @seealso See [example_data] for examples of these functions used in context.
#'
#' @rdname control_stack
#' @family control
#' @export
control_stack_grid <- function() {
  tune::control_grid(
    save_pred = TRUE,
    extract = stack_workflow
  )
}

#' @rdname control_stack
#' @export
control_stack_resamples <- function() {
  tune::control_resamples(
    save_pred = TRUE,
    extract = stack_workflow
  )
}

#' @rdname control_stack
#' @export
control_stack_bayes <- function() {
  tune::control_bayes(
    save_pred = TRUE,
    extract = stack_workflow
  )
}





