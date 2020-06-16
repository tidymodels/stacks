#' Add or remove stack members
#'
#' Add or remove models from a model stack specification.
#'
#' @param stack A model `stack` object.
#' @param member A `tune_results` object (or, if removing, its name)
#'   outputted by [tune::tune_grid()] or [tune::tune_bayes()].
#' @param ...
#' 
#' @return A model `stack` object--see [stack_init()] for more details! 
#' 
#' @examples 
#' # these examples make use of the svm_res_ and 
#' # spline_res_ objects exported with the package.
#' 
#' # initialize a model stack
#' st <- stack_init()
#' 
#' # add some members to the stack
#' st <- st %>%
#'   stack_add(svm_res_) %>%
#'   stack_add(spline_res_)
#' 
#' # remove the spline regression stack member
#' st <- st %>%
#'   stack_rm("spline_res_")
#' 
#' @name add_rm
NULL
