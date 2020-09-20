# this script imports and cleans raw data from Julie Jung's work on
# how red-eyed tree frogs sense predator vibrations

# load packages
library(tidyverse)
library(janitor)
library(usethis)
  
# read in raw data
tree_frogs_raw <- read_csv("data-raw/tree_frogs_raw.csv")

# set randomness (for reordering)
set.seed(2)

# some filtering + selecting :-)
tree_frogs <-
  tree_frogs_raw %>%
  dplyr::rename_all(janitor::make_clean_names) %>%
  dplyr::filter(
    experiment == "Individual"
  ) %>%
  dplyr::transmute(
    clutch = factor(clutch),
    age = age_groups_three,
    stimulus_time = (stimulus_h * 3600) + (stimulus_m * 60) + stimulus_s,
    treatment = case_when(
      treatment == "G" ~ "gentamicin",
      treatment == "NT" ~ "control",
      TRUE ~ NA_character_
    ),
    hatched = case_when(
      response == 1 ~ TRUE,
      response == 0 ~ FALSE,
      TRUE ~ NA
    ),
    hatch_time = (hatch_h * 3600) + (hatch_m * 60) + hatch_s
  ) %>%
  slice_sample(n = nrow(.))

usethis::use_data(tree_frogs, internal = FALSE, overwrite = TRUE)
