#' Add or remove stack members
#'
#' Add or remove members from a stack.
#'
#' @param stack A `stack` object.
#' @param member A `tune_results` object (or, if removing, its name)
#'   outputted by [tune::tune_grid()] or [tune::tune_bayes()].
#' @inheritParams new_stack
#' 
#' @return A `stack` object--see [new_stack()] for more details! 
#' 
#' @template note_example_data
#' 
#' @examples 
#' 
#' # initialize a model stack
#' st <- new_stack()
#' 
#' # add some members to the stack
#' st <- st %>%
#'   members_add(svm_res_) %>%
#'   members_add(spline_res_)
#' 
#' # remove the spline regression stack member
#' st <- st %>%
#'   members_rm("spline_res_")
#' 
#' @name add_rm
NULL

#' Example Data
#'
#' This package provides some \code{tune_results} objects for use in examples
#' and tests. [svm_res_] and [spline_res_] contain tuning results
#' for a support vector machine and spline model, respectively, fitting
#' \code{mpg} in the \code{mtcars} data using all of the other variables
#' as predictors. The source code for generating these objects is given below.
#' 
#' @examples 
#' \dontrun{
#' # setup: resample and basic recipe ------------------------
#' 
#' set.seed(1)
#' 
#' folds_ <- rsample::vfold_cv(mtcars, v = 3)
#' 
#' ctrl <- control_grid(save_pred = TRUE)
#' 
#' car_rec_ <- 
#'   recipes::recipe(mpg ~ ., data = mtcars) %>%
#'   recipes::step_normalize(recipes::all_predictors())
#' 
#' # support vector machine ----------------------------------
#' 
#' svm_mod_ <- 
#'   parsnip::svm_rbf(
#'     cost = tune::tune(), 
#'     rbf_sigma = tune::tune()
#'   ) %>%
#'   parsnip::set_engine("kernlab") %>%
#'   parsnip::set_mode("regression")
#' 
#' set.seed(1)
#' 
#' svm_res_ <- 
#'   tune::tune_grid(
#'     object = svm_mod_, 
#'     preprocessor = car_rec_, 
#'     resamples = folds_, 
#'     grid = 5,
#'     control = ctrl
#'   )
#' 
#' # spline regression ---------------------------------------
#' 
#' spline_rec_ <-
#'   recipes::recipe(mpg ~ ., data = mtcars) %>%
#'   recipes::step_ns(disp, deg_free = tune::tune("disp")) %>%
#'   recipes::step_ns(wt, deg_free = tune::tune("wt"))
#' 
#' lin_mod_ <-
#'   parsnip::linear_reg() %>%
#'   parsnip::set_engine("lm")
#' 
#' spline_grid_ <- expand.grid(disp = c(2, 4, 6), wt = c(2, 4, 6))
#' 
#' spline_res_ <- 
#'   tune::tune_grid(
#'     object = lin_mod_,
#'     preprocessor = spline_rec_, 
#'     resamples = folds_, 
#'     grid = spline_grid_,
#'     control = ctrl
#'   )
#' }
#' @name example_data
NULL

#' @rdname example_data
"svm_res_"

#' @rdname example_data
"spline_res_"
