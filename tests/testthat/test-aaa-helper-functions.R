# Helper Functions
# ------------------------------------------------------------------------
# returns the number of column names containing a substring
ncol_with_name <- function(data_stack, substring) {
  data_stack %>%
    tibble::as_tibble() %>%
    dplyr::select(dplyr::contains(!!substring)) %>%
    ncol()
}
