# Helper Functions
# ------------------------------------------------------------------------
# returns the number of column names containing a substring
ncol_with_name <- function(data_stack, substring) {
  data_stack |>
    tibble::as_tibble() |>
    dplyr::select(dplyr::contains(!!substring)) |>
    ncol()
}

# Helper Data
# -----------------------------------------------------------------------
# the `helper_data.Rda` contains data objects for use in unit testing.
# due to its size, it's not included in the built package.
# * when running locally, use the local helper_data.
# * when testing on a continuous integration platform, locate the helper_data
#   file in the appropriate reference environment
# * when on cran, only run the tests in test_cran that don't require the data.

get_current_branch <- function() {
  gh_ref <- Sys.getenv("GITHUB_REF")

  if (!identical(gh_ref, "")) {
    gsub("refs/heads/", "", gh_ref)
  } else {
    NA
  }
}

on_github <- function() {
  !is.na(get_current_branch())
}

on_cran <- function() {
  !identical(Sys.getenv("NOT_CRAN"), "true")
}
