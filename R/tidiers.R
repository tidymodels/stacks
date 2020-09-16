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
#' # to use the `tidy` method, the appropriate glance
#' # methods must be loaded for the member model tidiers.
#' # in this example, they come from broom!
#' library(broom)
#' 
#' # build a regression model stack
#' st <-
#'   stacks() %>%
#'   add_candidates(reg_res_lr) %>%
#'   add_candidates(reg_res_sp) %>%
#'   blend_predictions() %>%
#'   fit_members()
#'   
#' # check the model stack out!
#' tidy(st)
#' glance(st)
#' augment(st)
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
glance.model_stack <- function(x, ...) {
  tibble::tibble(
    "r.squared" = NA_real_, 
    "adj.r.squared" = NA_real_, 
    "sigma" = NA_real_, 
    "statistic" = NA_real_, 
    "p.value" = NA_real_, 
    "df" = NA_integer_, 
    "logLik" = NA_real_, 
    "AIC" = NA_real_, 
    "BIC" = NA_real_, 
    "deviance" = NA_real_, 
    "df.residual" = NA_integer_, 
    "nobs" = nrow(x[["train"]])
  )
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
tidy.model_stack <- function(x, ...) {
  glanced_model_stack <- glance(x)
  
  glanced_members <- 
    purrr::map(
      x[["member_fits"]],
      safe_tidy
    )
  
  if (any(!purrr::map_lgl(glanced_members, is.null))) {
    glanced_members %>%
    tibble::enframe() %>%
    dplyr::bind_rows(
      tibble::tibble(name = "ensemble", value = list(glanced_model_stack))
    ) %>%
    tidyr::unnest(cols = "value") %>%
    dplyr::rename(member = name)
  } else {
    tibble::tibble()
  }
}

# --------------------------------------------------------------------------

# a function to safely attempt to run the appropriate tidy method on each 
# member model. if the tidy method errors out, returns NULL
safe_tidy <- function(model_object) {
  fit <- model_object[["fit"]][["fit"]][["fit"]]
  
  tidied <-
    tryCatch(
      glance(fit),
      error = identity
    )
  
  if (inherits(tidied, "data.frame")) {
    return(tidied)
  } else {
    glue_message(
      "No glance method found for the `{list(class(model_object))}` class ",
      "model object. Please load the library with the appropriate tidiers ",
      "(broom, possibly) for this model object for its results to included ",
      "in the output. \n\n"
    )
    
    return(NULL)
  }
}
