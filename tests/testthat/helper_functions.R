# Helper Functions
# ------------------------------------------------------------------------
# returns the number of column names containing a substring
ncol_with_name <- function(data_stack, substring) {
  data_stack %>%
    tibble::as_tibble() %>%
    dplyr::select(dplyr::contains(!!substring)) %>%
    ncol()
}

# copied from utils
check_inherits <- function(x, what) {
  cl <- match.call()
  
  if (!inherits(x, what)) {
    glue_stop("Element `{list(cl$x)}` needs to inherit from `{what}`, but its ",
              "class is `{list(class(x))}`.")
  }
  
  invisible(TRUE)
}
