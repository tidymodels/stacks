#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' Plot an evaluated model stack 
#' 
#' @param object An evaluated model stack.
#' @inheritParams tune::autoplot.tune_results
#' 
#' @rdname autoplot
#' @export
autoplot.stack <- function(object, 
                           type = c("marginals", "parameters", "performance"),
                           metric = NULL, width = NULL, ...) {
  tune::autoplot(object$coefficients, type, metric, width, ...)
}