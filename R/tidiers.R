# broom tidying methods

# --------------------------------------------------------------------------------

#' @importFrom generics augment
#' @export
generics::augment

#' Augment data with information from a `model_stack` object
#' 
#' Augment accepts a model object and a dataset and adds information about 
#' each observation in the dataset. 
#' 
#' @param x A `model_stack` object
#' @param data A `data.frame`-like object containing all of the necessary
#' predictors that the model stack was trained with.
#' @inheritParams stacks
#' 
#' @return A [tibble::tibble()] with additional information on each observation
#' appended.
#' 
#' @family tidiers
#' 
#' @template note_example_data
#' 
#' @examples
#' \donttest{
#' # see the "Example Data" section above for
#' # clarification on the data and tuning results
#' # objects used in these examples!
#' 
#' # build a regression model stack
#' reg_st <-
#'   stacks() %>%
#'   stack_add(reg_res_lr) %>%
#'   stack_add(reg_res_sp) %>%
#'   stack_blend() %>%
#'   stack_fit()
#'   
#' # check the model stack out!
#' tidy(reg_st)
#' glance(reg_st)
#' augment(reg_st)
#' 
#' # build a classification model stack
#' class_st <-
#'   stacks() %>%
#'   stack_add(class_res_nn) %>%
#'   stack_add(class_res_rf) %>%
#'   stack_blend() %>%
#'   stack_fit()
#' 
#' # check the model stack out!
#' tidy(class_st)
#' glance(class_st)
#' augment(class_st)
#' }
#' 
#' @importFrom generics augment
#' @method augment model_stack
#' @export augment.model_stack
#' @export
augment.model_stack <- function(x, data = x[["train"]], ...) {
  # implement a model.frame method for model_stacks for the data default. 
  
  data$.fitted <- predict(x, data)
  
  data
}

# --------------------------------------------------------------------------------

#' @importFrom generics glance
#' @export
generics::glance

#' Glance at a `model_stack` object
#' 
#' Glance accepts a model object and returns a tibble::tibble() with 
#' one row of model summaries
#' 
#' @param x A `model_stack` object
#' @param data A `data.frame`-like object containing all of the necessary
#' predictors that the model stack was trained with.
#' @inheritParams stacks
#' 
#' @return A one-row [tibble::tibble()] with model-level summaries. Output
#' columns will depend on the model stack's mode.
#' 
#' @family tidiers
#' 
#' @template note_example_data
#' 
#' @inherit augment.model_stack
#' 
#' @importFrom generics glance
#' @method glance model_stack
#' @export glance.model_stack
#' @export
glance.model_stack <- function(x, data = x[["train"]], ...) {
  invisible(TRUE)
}

# --------------------------------------------------------------------------------

#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidy a `model_stack` object
#' 
#' @description
#' 
#' Tidy accepts a model object and summarizes information about the
#' components of a model. In the case of model stacks, these "components"
#' are the trained members.
#' 
#' Please note that this method is only implemented for model stacks
#' with _fitted_ members!
#' 
#' @param x A `model_stack` object
#' @inheritParams stacks
#' 
#' @return A [tibble::tibble()] a tibble with as many rows as
#' there are members, where each row gives `glance`-like results for that
#' model, as well as the stacking coefficients for those members. Output
#' columns will vary by the model stack's mode as well as the availability
#' of the members' tidying methods.
#' 
#' @family tidiers
#' 
#' @template note_example_data
#' 
#' @inherit augment.model_stack
#' 
#' @importFrom generics tidy
#' @method tidy model_stack
#' @export tidy.model_stack
#' @export
tidy.model_stack <- function(x, data = x[["train"]], ...) {
  invisible(TRUE)
}