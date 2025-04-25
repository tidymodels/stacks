# this script imports the raw data from https://doi.org/10.5061/dryad.4781n
# and tidies/cleans to form a dataset `caterpillar` for use in examples

# load packages
library(tidyverse)
library(readxl)

# read in the raw data
cat_height_raw <- read_excel("data-raw/caterpillars_raw.xlsx", sheet = "HEIGHT")
cat_spines_raw <- read_excel("data-raw/caterpillars_raw.xlsx", sheet = "SPINES")
cat_time_raw <- read_excel("data-raw/caterpillars_raw.xlsx", sheet = "TIME")

# pollination, progeny - self v hybrid

# process each similarly for col-binding
cat_height <-
  cat_height_raw |>
  tidyr::pivot_longer(
    tidyselect::everything(),
    values_to = "height",
    names_to = "progeny"
  ) |>
  tidyr::drop_na(height) |>
  dplyr::mutate(
    progeny = stringr::str_remove(progeny, " height"),
    progeny = case_when(
      progeny == "inbred" ~ "self",
      TRUE ~ "hybrid"
    ),
    height = case_when(
      height < 25 ~ "short",
      height > 30 ~ "tall",
      TRUE ~ "medium"
    )
  )

cat_spines <-
  cat_spines_raw |>
  tidyr::pivot_longer(
    tidyselect::everything(),
    values_to = "spines",
    names_to = "progeny"
  ) |>
  tidyr::drop_na(spines) |>
  dplyr::mutate(
    progeny = stringr::str_remove(progeny, " spines"),
    progeny = case_when(
      progeny == "inbred" ~ "self",
      TRUE ~ "hybrid"
    )
  )

cat_time <-
  cat_time_raw |>
  tidyr::pivot_longer(
    tidyselect::everything(),
    values_to = "time",
    names_to = "progeny"
  ) |>
  tidyr::drop_na(time) |>
  dplyr::mutate(
    progeny = stringr::str_remove(progeny, " time"),
    progeny = case_when(
      progeny == "inbred" ~ "self",
      TRUE ~ "hybrid"
    )
  )

caterpillars <-
  bind_cols(cat_height, spines = cat_spines$spines, time = cat_time$time)
