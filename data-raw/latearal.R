# this script imports and cleans raw data from Julie Jung's work on
# red-eyed tree frog

# load packages
library(tidyverse)
library(janitor)
library(usethis)
  
# read in raw data
latearal_raw <- read_csv("data-raw/latearal_raw.csv")

# set randomness (for reordering)
set.seed(2)

# some filtering and cleaning up
latearal <-
  latearal_raw %>%
  dplyr::rename_all(janitor::make_clean_names) %>%
  dplyr::filter(experiment == "Individual") %>%
  dplyr::transmute(
    clutch,
    age = age_groups_three,
    stimulus_h,
    stimulus_m,
    stimulus_s,
    treatment = tolower(treatment),
    hatched = case_when(
      response == 0 ~ TRUE,
      response == 1 ~ FALSE,
      TRUE ~ NA
    )
  ) %>%
  slice_sample(n = 500)

