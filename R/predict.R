#' Model predictions
#'
#' Apply a model stack to create different types of predictions.
#'
#' @param object A model stack with fitted members.
#' @param new_data A rectangular data object, such as a data frame.
#' @inheritParams stacks
#'
#' @importFrom stats predict
#' @method predict model_stack
#' @export predict.model_stack
#' @export
predict.model_stack <- function(object, new_data, ...) {
  invisible(TRUE)
}
